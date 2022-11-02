require(ggplot2)
require(openxlsx)
require(xml2)
require(zip)
require(dplyr)
require(stringr)
require(Rcpp)

Read_Filled_Sheet <- function() {
  #TODO check to make sure sheet looks filled in! or at least check resulting data should be easy
  df = readWorkbook(xlsxFile = "supervisor.xlsx", sheet = 1, cols = c(1, 4, 6, 9, 12, 15, 18), colNames = FALSE)
  View(df)
  #then filter to only rows where rat names match archive, which means we need real rat name still present either in column 1 or in some other column that we need to fetch instead of 1
}

Define_Styles <- function() {
  rat_name_style <<- createStyle(fontSize = 20, textDecoration = "bold")
  rat_header_style <<- createStyle(valign = "center", wrapText = TRUE)
  date_style <<- createStyle(halign = "right")
  mandatory_input_reject_style <<- createStyle(bgFill = "#FFE699", fontColour = "#9C5700")  #conditional uses bg
  mandatory_input_accept_style <<- createStyle(bgFill = "#C6EFCE", fontColour = "#006100")  #conditional uses bg
  optional_input_style <<- createStyle(fgFill = "#FFFFCC", fontColour = "#9C5700")          #regular uses fg
  warning_style <<- createStyle(bgFill = "#FFC7CE", fontColour = "#9C0006")                 #conditional uses bg
  halign_center_style <<- createStyle(halign = "center")
  table_header_style <<- createStyle(fgFill = "darkgray", fontColour = "white", textDecoration = "bold")
}

Setup_Workbook <- function() {

  wb <<- createWorkbook()

  options("openxlsx.borderColour" = "#4F80BD")
  options("openxlsx.borderStyle" = "thin")
  modifyBaseFont(wb, fontSize = 10, fontName = "Calibri")

  addWorksheet(wb, sheetName = "Summary", tabColour = "limegreen")

  setColWidths(wb, 1, cols = "A", widths = 20)
  setColWidths(wb, 1, cols = 2:3, widths = 10)
  setColWidths(wb, 1, cols = "D", widths = 25)
  setColWidths(wb, 1, cols = 5:27, widths = 5)
  setColWidths(wb, 1, cols = 18:27, widths = 5, hidden = FALSE) # hide reserved-for-future columns?
  setColWidths(wb, 1, cols = "AB", widths = 16, hidden = FALSE)
  setColWidths(wb, 1, cols = "AC", widths = 50)

  # add experimental configurations table from external file
  experiment_config_df = readWorkbook(xlsxFile = "experiment details.xlsx", sheet = 1)
  max_search_rows <<- nrow(experiment_config_df)
  writeData(wb, 1, experiment_config_df, startRow = settings$config_row, startCol = settings$config_col, colNames = FALSE, rowNames = FALSE)

  return(wb)
}

Write_Header <- function() {
  Calculate_Next_Run_Text <- function () {
    # TODO this should be even smarter and determine if there are assignments for today
    # If not, and today is not sunday, then assign for today
    # Basically it should advance to the first non-sunday that's unassigned
    # But to do any of that I need to know where I'm putting assignment data. Probably rat_archive.
    nextrun = Sys.Date() + 1
    nextrun_text = "Tomorrow"
    if (nextrun %>% format("%A") == "Sunday") {
      nextrun = nextrun + 1
      nextrun_text = "Monday"
    }
    nextrun_text = paste0(nextrun_text, " - ", nextrun %>% format("%m/%d/%Y"))
    return(nextrun_text)
  }

  Set_Height_Main_Row <- function() {
    warning_count = length(run$warnings_list[[1]])
    height_by_count = 13*warning_count+2
    min_height = 28
    height_to_set = max(min_height, height_by_count)
    setRowHeights(wb, 1, rows = row, heights = height_to_set)
  }

  Write_Dynamic_Lists <- function() {
    Build_List <- function(i) {
      #build the excel formula that will display the items from the output range that correspond to the query cell for the input range
      dynamic_list_formula = paste0("=IFERROR(INDEX(", output_range, ",SMALL(IF(", query_cell, "=", input_range, ",ROW(", input_range, ")-ROW(", range_start, ")+1),ROW(", i, ":", i, "))),\"\")")
      writeFormula(wb, 1, dynamic_list_formula, startRow = settings$config_row+i, startCol = settings$dynamic_col + col_offset, array = TRUE)
    }

    # phases list lives at settings$config_col+1 (experiment) and +2 (phase), querying off experiment in F
    range_start = getCellRefs(data.frame(settings$config_row, settings$config_col + 2))
    range_end = getCellRefs(data.frame(settings$config_row + max_search_rows, settings$config_col + 2))
    output_range = paste0(range_start, ":", range_end)
    range_start = getCellRefs(data.frame(settings$config_row, settings$config_col + 1)) # going to use this when we build formula, so it has to be second
    range_end = getCellRefs(data.frame(settings$config_row + max_search_rows, settings$config_col + 1))
    input_range = paste0(range_start, ":", range_end)
    query_cell = getCellRefs(data.frame(row, 6))
    col_offset = 0
    sapply(c(1:settings$dynamic_list_length), Build_List)

    #task list lives at settings$config_col+3 (phase) and +4 (task), querying from phase in I
    range_start = getCellRefs(data.frame(settings$config_row, settings$config_col + 4))
    range_end = getCellRefs(data.frame(settings$config_row + max_search_rows, settings$config_col + 4))
    output_range = paste0(range_start, ":", range_end)
    range_start = getCellRefs(data.frame(settings$config_row, settings$config_col + 3)) # going to use this when we build formula, so it has to be second
    range_end = getCellRefs(data.frame(settings$config_row + max_search_rows, settings$config_col + 3))
    input_range = paste0(range_start, ":", range_end)
    query_cell = getCellRefs(data.frame(row, 9))
    col_offset = col_offset + 1
    sapply(c(1:settings$dynamic_list_length), Build_List)

    #detail list lives at settings$config_col+5 (phase again) and +6 (detail), querying from phase again in I
    range_start = getCellRefs(data.frame(settings$config_row, settings$config_col + 6))
    range_end = getCellRefs(data.frame(settings$config_row + max_search_rows, settings$config_col + 6))
    output_range = paste0(range_start, ":", range_end)
    range_start = getCellRefs(data.frame(settings$config_row, settings$config_col + 5)) # going to use this when we build formula, so it has to be second
    range_end = getCellRefs(data.frame(settings$config_row + max_search_rows, settings$config_col + 5))
    input_range = paste0(range_start, ":", range_end)
    query_cell = getCellRefs(data.frame(row, 9))
    col_offset = col_offset + 1
    sapply(c(1:settings$dynamic_list_length), Build_List)

  }

  # Header Workflow ---------------------------------------------------------

  Write_Dynamic_Lists()

  #Rat Name
  rat_name = run$rat_name %>% stringr::str_to_upper(string = .)
  digit_index = str_locate(string = rat_name, pattern = "[:digit:]")[1]
  rat_name = paste0(stringr::str_sub(rat_name, 1, digit_index - 1),
                    " ",
                    stringr::str_sub(rat_name, digit_index, stringr::str_length(rat_name))
  )
  addStyle(wb, 1, rat_name_style, rows = row, cols = 1)
  #TODO should we stick the unmodified rat name in e.g. column AB to make it easier to retrieve later?

  #Date
  addStyle(wb, 1, date_style, rows = row, cols = 2:3)
  mergeCells(wb, 1, cols = 2:3, rows = row)
  nextrun_text = Calculate_Next_Run_Text()

  #Tomorrow's Filename
  conditionalFormatting(wb, 1, type = "contains", rule = "[[", style = mandatory_input_reject_style, rows = row, cols = 4)
  conditionalFormatting(wb, 1, type = "notcontains", rule = "[[", style = mandatory_input_accept_style, rows = row, cols = 4)

  #Experiment
  range_start = getCellRefs(data.frame(settings$config_row, settings$config_col))
  range_end = getCellRefs(data.frame(settings$config_row+7, settings$config_col))
  range_string = paste0(range_start, ":", range_end)
  dataValidation(wb, 1, rows = row, cols = 6, type = "list", value = range_string, operator = "")
  mergeCells(wb, 1, cols = 6:7, rows = row)
  rule_string = paste0("COUNTIF(", range_string, ",F", row, ")>0")
  conditionalFormatting(wb, 1, rule = rule_string, style = mandatory_input_accept_style, rows = row, cols = 6)
  rule_string = paste0("COUNTIF(", range_string, ",F", row, ")<=0")
  conditionalFormatting(wb, 1, rule = rule_string, style = mandatory_input_reject_style, rows = row, cols = 6)

  #Experimental Phase
  range_start = getCellRefs(data.frame(row + 1, settings$dynamic_col))
  range_end = getCellRefs(data.frame(row + 1 + settings$dynamic_list_length, settings$dynamic_col))
  range_string = paste0(range_start, ":", range_end)
  dataValidation(wb, 1, rows = row, cols = 9, type = "list", value = range_string, operator = "")
  rule_string = paste0("COUNTIF(", range_string, ",I", row, ")>0")
  conditionalFormatting(wb, 1, rule = rule_string, style = mandatory_input_accept_style, rows = row, cols = 9)
  rule_string = paste0("COUNTIF(", range_string, ",I", row, ")<=0")
  conditionalFormatting(wb, 1, rule = rule_string, style = mandatory_input_reject_style, rows = row, cols = 9)
  mergeCells(wb, 1, cols = 9:10, rows = row)

  #Task
  range_start = getCellRefs(data.frame(row + 1, settings$dynamic_col + 1))
  range_end = getCellRefs(data.frame(row + 1 + settings$dynamic_list_length, settings$dynamic_col + 1))
  range_string = paste0(range_start, ":", range_end)
  dataValidation(wb, 1, rows = row, cols = 12, type = "list", value = range_string, operator = "")
  rule_string = paste0("COUNTIF(", range_string, ",L", row, ")>0")
  conditionalFormatting(wb, 1, rule = rule_string, style = mandatory_input_accept_style, rows = row, cols = 12)
  rule_string = paste0("COUNTIF(", range_string, ",L", row, ")<=0")
  conditionalFormatting(wb, 1, rule = rule_string, style = mandatory_input_reject_style, rows = row, cols = 12)
  mergeCells(wb, 1, cols = 12:13, rows = row)

  #Detail
  range_start = getCellRefs(data.frame(row + 1, settings$dynamic_col + 2))
  range_end = getCellRefs(data.frame(row + 1 + settings$dynamic_list_length, settings$dynamic_col + 2))
  range_string = paste0(range_start, ":", range_end)
  dataValidation(wb, 1, rows = row, cols = 15, type = "list", value = range_string, operator = "")
  rule_string = paste0("COUNTIF(", range_string, ",O", row, ")>0")
  conditionalFormatting(wb, 1, rule = rule_string, style = mandatory_input_accept_style, rows = row, cols = 15)
  rule_string = paste0("COUNTIF(", range_string, ",O", row, ")<=0")
  conditionalFormatting(wb, 1, rule = rule_string, style = mandatory_input_reject_style, rows = row, cols = 15)
  mergeCells(wb, 1, cols = 15:16, rows = row)

  #Global Comment Field
  addStyle(wb, 1, rows = row, cols = 18:27, style = optional_input_style) # center the warning text
  mergeCells(wb, 1, cols = 18:27, rows = row) # rat global comment merge

  #Warnings
  conditionalFormatting(wb, 1, type = "expression", rule = "==\"Warnings: none\"", style = mandatory_input_accept_style, rows = row, cols = 29)
  conditionalFormatting(wb, 1, type = "expression", rule = "!=\"Warnings: none\"", style = warning_style, rows = row, cols = 29)
  addStyle(wb, 1, rows = row, cols = 29, style = halign_center_style, stack = TRUE) # center the warning text

  # openxlsx has trouble writing linebreaks to a single cell
  # so instead, above in Parse_Warnings we write them tab-separated
  # This excel formula then reads the tab-sep cell and formats it with linebreaks, as a workaround
  v = c("IF(LEN(AB3)=0,\"Warnings: none\", SUBSTITUTE(AB3, \"	\", \"\n\"))") #TODO change AB3 hardcoded to tablerow etc
  writeFormula(wb, 1, x = v, startRow = row, startCol = 29)


  #Entire Main Row
  addStyle(wb, 1, rows = row, cols = 1:29, style = rat_header_style, stack = TRUE) # vertically center & wrap the main row
  Set_Height_Main_Row()

  rat_header_df = data.frame(
    rat_name, #A
    nextrun_text, #B
    "", #C - merge with previous
    "[Tomorrow's Filename]", #D
    "", #E
    "[Experiment]", #F
    "", #G - merge with previous
    "", #H
    "[Phase]", #I
    "", #J - merge with previous
    "", #K
    "[Task]", #L,
    "", #M - merge with previous
    "", #N
    "[Detail]", #O,
    "", #P - merge with previous
    "", #Q
    "[Global comment field e.g. week-ahead informal plan for this rat]", #R
    "", "", "", "", "", #S T U V W
    "", "", "", "", "", #X Y Z AA AB
    check.names = FALSE, fix.empty.names = FALSE
  )

  writeData(wb, 1, x = rat_header_df, startRow = row, colNames = FALSE, rowNames = FALSE)
}

Build_Row <- function(run) {
  Parse_Warnings <- function () {
    r = run$warnings_list[[1]] %>% unlist() %>% stringr::str_c(collapse = "\t")
    return(r)
  }

  colA_phase = "Some Trial Phase Name" #TODO
  colB_blank = ""
  colC_date = paste0(substr(run$date, 5, 6), "/", substr(run$date, 7, 8), "/",substr(run$date, 1, 4))
  today = Sys.Date() %>% format("%m/%d/%Y")
  if(colC_date == today) {
    colC_date = "Today"
  }
  colD_filename = run$file_name
  colE_trials = run$stats[[1]]$trial_count
  colF_hitpercent = run$stats[[1]]$hit_percent
  colG_FApercent = run$stats[[1]]$FA_percent
  colH_dprime = "" #run$stats[[1]]$dprime
  colI_react = "" #run$stats[[1]]$reaction_time
  colJ_thresh = "" #run$stats[[1]]$threshold
  #then cols L-Q depending on task, right? I think NG FA% x Freq is only for particular experiment/phase

  df = data.frame(colA_phase, colB_blank, colC_date, colD_filename, colE_trials, colF_hitpercent, colG_FApercent, colH_dprime, colI_react, colJ_thresh,
                  "","","","","","","","","","","","","","","","","",Parse_Warnings(), run$comments,
                  check.names = FALSE, fix.empty.names = FALSE)
  df = df %>% rbind(df)

  return(df)
}

Build_Table <- function() {
  Define_Table <- function() {
    df = data.frame(matrix(ncol = 29, nrow =0))
    colnames(df) = c(LETTERS, "AA", "AB", "AC")
    addStyle(wb, 1, table_header_style, rows = row, cols = 1:29, gridExpand = TRUE)
    return(df)
  }

  Finalize_Table <- function () {
    # the obnoxious blank strings are because every column in a table has to have a unique header,
    # and because we want those headers to be blank for all but column A
    colnames(df_table) = c("Choose Filter", " ", "  ", "   ", "    ", "     ", "      ", "       ", "        ", "         ", "          ", "           ", "            ", "             ", "              ", "               ", "                ", "                 ", "                  ", "                   ", "                    ", "                     ", "                      ", "                       ", "                        ", "                         ", "                          ", "                           ", "                            ")
    return(df_table)
  }

  Get_Task_List <- function() {
    run_today = rat_runs %>% dplyr::arrange(date) %>% tail(1)
    experiment_current = run_today$assignment[[1]]$experiment
    phase_current = run_today$assignment[[1]]$phase
    r = rat_runs %>%
      filter(map_lgl(assignment, ~ .x$experiment == experiment_current)) %>%
      filter(map_lgl(assignment, ~ .x$phase == phase_current)) %>%
      .$assignment %>% bind_rows() %>% .$task %>% unique()
    return(r)
  }

  foo <- function() {
    rat_runs = run_archive %>% dplyr::filter(rat_ID == ratID)
    run_today = rat_runs %>% dplyr::arrange(date) %>% tail(1)
    experiment_current = run_today$assignment[[1]]$experiment
    phase_current = run_today$assignment[[1]]$phase

    # Common Columns ----------------------------------------------------------
    columns = c("task", "detail", "date", "file_name", "weight", "trial_count", "hit_percent", "FA_percent", "mean_attempts_per_trial", "threshold", "reaction")

    r = rat_runs %>%
      dplyr::filter(map_lgl(assignment, ~ .x$experiment == experiment_current)) %>%
      dplyr::filter(map_lgl(assignment, ~ .x$phase == phase_current)) %>%
      tidyr::unnest_wider(assignment) %>%
      tidyr::unnest_wider(stats) %>%
      dplyr::mutate(date = paste0(stringr::str_sub(date, 5, 5) %>% stringr::str_replace(pattern = "0", replacement = ""),
                                  stringr::str_sub(date, 6, 6), "/",
                                  stringr::str_sub(date, 7, 7) %>% stringr::str_replace(pattern = "0", replacement = ""),
                                  stringr::str_sub(date, 8, 8), "/", stringr::str_sub(date, 1, 4))) %>%
      group_by(task, detail) %>%
      do(
        dplyr::arrange(., dplyr::desc(date)) %>% dplyr::select(columns)
      )

    weight_max = max(rat_runs$weight) # Rat_runs not r because we want all history, not just days corresponding to this experiment/phase

    # Task-specific RXN column ------------------------------------------------

    if(experiment_current == "Oddball") {
      #stuff
    } else {
      if(phase_current == "Octave") {
        #stuff
      } else {
        duration = r %>% unnest(reaction) %>% .$`Dur (ms)` %>% unique() %>% min()

        # TH and Rxn refer to the task detail not the column
        df_TH_BBN = NULL
        df_TH_tones = NULL
        df_Rxn = NULL

        df_TH_BBN = r %>% unnest(reaction) %>%
          dplyr::filter(task == "TH" & `Dur (ms)` == duration & `Freq (kHz)` == 0 & `Inten (dB)` == 40)

        intensity = r %>% unnest(reaction) %>%
          dplyr::filter(task == "TH" & `Dur (ms)` == duration & `Freq (kHz)` != 0) %>% #select(-threshold, -file_name, - weight, -mean_attempts_per_trial) %>% View
          group_by(date) %>%
          count(`Inten (dB)`) %>% arrange(desc(`Inten (dB)`)) %>% slice(which.max(n)) %>%
          rename(desired_dB = `Inten (dB)`) %>% select(-n)

        df_TH_tones = r %>% unnest(reaction) %>%
          dplyr::filter(task == "TH" & `Dur (ms)` == duration & `Freq (kHz)` != 0) %>% #select(-threshold, -file_name, - weight, -mean_attempts_per_trial) %>% View
          right_join(intensity, by = "date") %>%
          filter(`Inten (dB)` == desired_dB) %>%
          group_by(date) %>%
          mutate(Rxn = mean(Rxn)) %>%
          select(-desired_dB)

        df_Rxn = r %>% unnest(reaction) %>%
          dplyr::filter(task != "TH" & `Dur (ms)` == duration & `Inten (dB)` == 60) %>% # not equal TH
          group_by(date) %>%
          mutate(Rxn = mean(Rxn))

        df = rbind(df_TH_BBN, df_TH_tones, df_Rxn) %>%
          select(-`Freq (kHz)`, -`Dur (ms)`, -`Inten (dB)`) %>%
          distinct()


      }

    }


      View

    # Phase-specific Columns --------------------------------------------------------
    if(phase_current == "Tones") {

    }

    averages = r %>%
      # dplyr::group_by(task, detail) %>% #TODO also by detail
      dplyr::select(-date, -file_name, -threshold, -reaction) %>%
      dplyr::summarise_all(mean) %>%
      dplyr::mutate(date = "Overall", file_name = "Averages") %>%
      dplyr::relocate(date, file_name, .before = weight)

    r = r %>% do(
      head(., 3)
    ) %>%
      mutate(date = as.character(date))

    r = rbind(averages, r) %>%
      mutate(weight = (weight - weight_max)/weight_max)

  }

  Build_Counts <- function() {
    # NOELLE
    # get all phases (slice run_archive by rat_ID and rat_archive$rat_ID$current_experiment? OR grab experiment and then associated phases from some experiment_archive)
    # then get obs counts per phase
    # then append most recent date of phase
    # then grab today's observations and put them in place
    # then fold into df_table


  }



  Add_Task_to_Table <- function(task) {
    # NOELLE grab first phase, either from df_table or as above
    #...more steps needed, this isn't finished

    # BRIAN left off here, gonna be like.. write the header and then the rows and update row counter
    # probably dplyr from a premade df of the headers, complete with phase name in the first column, simply filter and paste intact
    # eh.. while that's great for the header it doesn't do the hard part which is the calculating, and hides the labels of what we're calculating, so maybe not.
    # we still have to case statement to do the calculated parts, so it doesn't even save that. nevermind.
  }

  row = row+1 #now points at table start row
  row_table_start = row #save for later
  df_table = Define_Table()




  df_table = Build_Row(run_archive[1,]) #TODO more than one run, selected by rat_ID


  df_table = Finalize_Table()
  writeDataTable(wb, 1, x = df_table, startRow = row_table_start, colNames = TRUE, rowNames = FALSE, bandedRows = FALSE, tableStyle = "TableStyleMedium18")

}

Add_Rat_To_Workbook <- function(row, rat_ID) {
  # NOTE we aren't doing anything special for handling multiple runs in the same day - they have a warning and we display recent run history, that's enough
  run <<- run_archive[1,]

  # write the header
  Write_Header()

  # then write the table
  Build_Table()
  #df = data.frame(x = "foo", y = "bar") #overwriting into table works fine
  #writeData(wb, 1, df, startRow = 3, startCol = 5, colNames = FALSE)
}



# Supervisor Workflow -----------------------------------------------------------
ratID = 24 #TODO more than one rat
row = 1 #global, index of the next unwritten row
settings = list(dynamic_list_length = 7, dynamic_col = 56, config_row = 1, config_col = 64)

Define_Styles()
#df = Build_Row()
Setup_Workbook()
Add_Rat_To_Workbook(row, rat_ID)
openXL(wb)
#saveWorkbook(wb, "supervisor.xlsx", overwrite = TRUE)
