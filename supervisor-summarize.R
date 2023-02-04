# data loading external file formats
library(R.matlab); library(openxlsx); library(xml2); library(zip);

# data manipulation
library(tidyverse); library(dplyr); library(tidyr); library(rlang); library(stringr); library(purrr); library(data.table)

InitializeWriter <- function() {
  options(warn=1) # we want to display warnings as they occur, so that it's clear which file caused which warnings

  source(paste0(projects_folder, "settings.R"))  # user variables

  experiment_config_df <<- read.csv(paste0(projects_folder, "experiment_details.csv"), na.strings = "N/A")
  experiment_config_df <<- Filter(function(x)!all(is.na(x)), experiment_config_df) # remove NA columns

  rat_archive <<- read.csv(paste0(projects_folder, "rat_archive.csv"), na.strings = c("N/A","NA"))
  load(paste0(projects_folder, "run_archive.Rdata"), .GlobalEnv)
}

Workbook_Writer <- function() {
  Define_Styles <- function() {
    rat_name_style <<- createStyle(fontSize = 22, textDecoration = "bold")
    rat_header_style <<- createStyle(valign = "center", wrapText = TRUE)
    date_style <<- createStyle(halign = "right")
    mandatory_input_reject_style <<- createStyle(bgFill = "#FFE699", fontColour = "#9C5700")  #conditional uses bg
    mandatory_input_accept_style <<- createStyle(bgFill = "#C6EFCE", fontColour = "#006100")  #conditional uses bg
    optional_input_style <<- createStyle(fgFill = "#FFFFCC", fontColour = "#9C5700")          #regular uses fg
    warning_style <<- createStyle(bgFill = "#FFC7CE", fontColour = "#9C0006")                 #conditional uses bg
    halign_center_style <<- createStyle(halign = "center")
    table_header_style <<- createStyle(fgFill = "darkgray", fontColour = "white", textDecoration = "bold") #regular uses fg
    key_style <<- createStyle(textDecoration = "bold")
    key_center <<- createStyle (textDecoration = "bold", halign = "center")
    key_merged_style <<- createStyle(fgFill = "#D9D9D9", textDecoration = "bold", halign = "left") #regular uses fg
    averages_style <<- createStyle(textDecoration = "italic")
    today_style <<- createStyle(fontColour = "#ED7D31")
    percent_style <<- createStyle(numFmt = "0%")
  }

  Setup_Workbook <- function() {
    wb <<- createWorkbook()
    rowCurrent <<- 1 #persistent, index of the next unwritten rowCurrent

    options("openxlsx.borderColour" = "#4F80BD")
    options("openxlsx.borderStyle" = "thin")
    modifyBaseFont(wb, fontSize = 10, fontName = "Calibri")

    addWorksheet(wb, sheetName = "Summary", tabColour = "limegreen")

    setColWidths(wb, 1, cols = "A", widths = 20)
    setColWidths(wb, 1, cols = "B", widths = 9)
    setColWidths(wb, 1, cols = "C", widths = 10)
    setColWidths(wb, 1, cols = "D", widths = 34)
    setColWidths(wb, 1, cols = 5:27, widths = 5)
    setColWidths(wb, 1, cols = 18:27, widths = 5, hidden = FALSE)
    setColWidths(wb, 1, cols = "AB", widths = 5, hidden = FALSE)
    setColWidths(wb, 1, cols = "AC", widths = 53.71)

    # add experimental configurations table from external file
    max_search_rows <<- nrow(experiment_config_df)
    writeData(wb, 1, experiment_config_df, startRow = user_settings$config_row, startCol = user_settings$config_col, colNames = FALSE, rowNames = FALSE)

    return(wb)
  }

  Add_Rat_To_Workbook <- function(ratID) {
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

      Write_Dynamic_Lists <- function() {
        Build_List <- function(i) {
          #build the excel formula that will display the items from the output range that correspond to the query cell for the input range
          dynamic_list_formula = paste0("=IFERROR(INDEX(", output_range, ",SMALL(IF(", query_cell, "=", input_range, ",ROW(", input_range, ")-ROW(", range_start, ")+1),ROW(", i, ":", i, "))),\"\")")
          writeFormula(wb, 1, dynamic_list_formula, startRow = user_settings$config_row+i+rowCurrent-1, startCol = user_settings$dynamic_col + col_offset, array = TRUE)
        }

        r = user_settings$config_row
        c = user_settings$config_col

        # phases list lives at c+1 (experiment) and +2 (phase), querying off experiment in F
        range_start = getCellRefs(data.frame(r, c + 2))
        range_end = getCellRefs(data.frame(r + max_search_rows, c + 2))
        output_range = paste0(range_start, ":", range_end)
        range_start = getCellRefs(data.frame(r, c + 1)) # going to use this when we build formula, so it has to be second
        range_end = getCellRefs(data.frame(r + max_search_rows, c + 1))
        input_range = paste0(range_start, ":", range_end)
        query_cell = getCellRefs(data.frame(rowCurrent, 6))
        col_offset = 0
        sapply(c(1:user_settings$dynamic_list_length), Build_List)

        #task list lives at c+3 (phase) and +4 (task), querying from phase in I
        range_start = getCellRefs(data.frame(r, c + 4))
        range_end = getCellRefs(data.frame(r + max_search_rows, c + 4))
        output_range = paste0(range_start, ":", range_end)
        range_start = getCellRefs(data.frame(r, c + 3)) # going to use this when we build formula, so it has to be second
        range_end = getCellRefs(data.frame(r + max_search_rows, c + 3))
        input_range = paste0(range_start, ":", range_end)
        query_cell = getCellRefs(data.frame(rowCurrent, 9))
        col_offset = col_offset + 1
        sapply(c(1:user_settings$dynamic_list_length), Build_List)

        #detail list lives at c+5 (phase again) and +6 (detail), querying from phase again in I
        range_start = getCellRefs(data.frame(r, c + 6))
        range_end = getCellRefs(data.frame(r + max_search_rows, c + 6))
        output_range = paste0(range_start, ":", range_end)
        range_start = getCellRefs(data.frame(r, c + 5)) # going to use this when we build formula, so it has to be second
        range_end = getCellRefs(data.frame(r + max_search_rows, c + 5))
        input_range = paste0(range_start, ":", range_end)
        query_cell = getCellRefs(data.frame(rowCurrent, 9))
        col_offset = col_offset + 1
        sapply(c(1:user_settings$dynamic_list_length), Build_List)

      }

      # Header Workflow ---------------------------------------------------------

      Write_Dynamic_Lists()

      #Rat Name & ID
      rat_name = run_today$rat_name
      #rat_name = stringr::str_to_upper(string = rat_name)
      digit_index = str_locate(string = rat_name, pattern = "[:digit:]")[1]
      rat_name = paste0(stringr::str_sub(rat_name, 1, digit_index - 1),
                        " ",
                        stringr::str_sub(rat_name, digit_index, stringr::str_length(rat_name))
      )
      addStyle(wb, 1, rat_name_style, rows = rowCurrent, cols = 1) # Name
      addStyle(wb, 1, rat_name_style, rows = rowCurrent, cols = 30) # ID


      #Date
      addStyle(wb, 1, date_style, rows = rowCurrent, cols = 2:3)
      mergeCells(wb, 1, cols = 2:3, rows = rowCurrent)
      nextrun_text = Calculate_Next_Run_Text()

      #Tomorrow's Filename
      conditionalFormatting(wb, 1, type = "contains", rule = "[", style = mandatory_input_reject_style, rows = rowCurrent, cols = 4)
      conditionalFormatting(wb, 1, type = "notcontains", rule = "[", style = mandatory_input_accept_style, rows = rowCurrent, cols = 4)

      #Experiment
      range_start = getCellRefs(data.frame(user_settings$config_row, user_settings$config_col))
      range_end = getCellRefs(data.frame(user_settings$config_row + user_settings$dynamic_list_length, user_settings$config_col))
      range_string = paste0(range_start, ":", range_end)
      suppressWarnings(dataValidation(wb, 1, rows = rowCurrent, cols = 6, type = "list", value = range_string, operator = ""))
      mergeCells(wb, 1, cols = 6:7, rows = rowCurrent)
      rule_string = paste0("COUNTIF(", range_string, ",F", rowCurrent, ")>0")
      conditionalFormatting(wb, 1, rule = rule_string, style = mandatory_input_accept_style, rows = rowCurrent, cols = 6)
      rule_string = paste0("COUNTIF(", range_string, ",F", rowCurrent, ")<=0")
      conditionalFormatting(wb, 1, rule = rule_string, style = mandatory_input_reject_style, rows = rowCurrent, cols = 6)

      #Experimental Phase
      range_start = getCellRefs(data.frame(rowCurrent + 1, user_settings$dynamic_col))
      range_end = getCellRefs(data.frame(rowCurrent + 1 + user_settings$dynamic_list_length, user_settings$dynamic_col))
      range_string = paste0(range_start, ":", range_end)
      suppressWarnings(dataValidation(wb, 1, rows = rowCurrent, cols = 9, type = "list", value = range_string, operator = ""))
      rule_string = paste0("COUNTIF(", range_string, ",I", rowCurrent, ")>0")
      conditionalFormatting(wb, 1, rule = rule_string, style = mandatory_input_accept_style, rows = rowCurrent, cols = 9)
      rule_string = paste0("COUNTIF(", range_string, ",I", rowCurrent, ")<=0")
      conditionalFormatting(wb, 1, rule = rule_string, style = mandatory_input_reject_style, rows = rowCurrent, cols = 9)
      mergeCells(wb, 1, cols = 9:10, rows = rowCurrent)

      #Task
      range_start = getCellRefs(data.frame(rowCurrent + 1, user_settings$dynamic_col + 1))
      range_end = getCellRefs(data.frame(rowCurrent + 1 + user_settings$dynamic_list_length, user_settings$dynamic_col + 1))
      range_string = paste0(range_start, ":", range_end)
      suppressWarnings(dataValidation(wb, 1, rows = rowCurrent, cols = 12, type = "list", value = range_string, operator = ""))
      rule_string = paste0("COUNTIF(", range_string, ",L", rowCurrent, ")>0")
      conditionalFormatting(wb, 1, rule = rule_string, style = mandatory_input_accept_style, rows = rowCurrent, cols = 12)
      rule_string = paste0("COUNTIF(", range_string, ",L", rowCurrent, ")<=0")
      conditionalFormatting(wb, 1, rule = rule_string, style = mandatory_input_reject_style, rows = rowCurrent, cols = 12)
      mergeCells(wb, 1, cols = 12:13, rows = rowCurrent)

      #Detail
      range_start = getCellRefs(data.frame(rowCurrent + 1, user_settings$dynamic_col + 2))
      range_end = getCellRefs(data.frame(rowCurrent + 1 + user_settings$dynamic_list_length, user_settings$dynamic_col + 2))
      range_string = paste0(range_start, ":", range_end)
      suppressWarnings(dataValidation(wb, 1, rows = rowCurrent, cols = 15, type = "list", value = range_string, operator = ""))
      rule_string = paste0("COUNTIF(", range_string, ",O", rowCurrent, ")>0")
      conditionalFormatting(wb, 1, rule = rule_string, style = mandatory_input_accept_style, rows = rowCurrent, cols = 15)
      rule_string = paste0("COUNTIF(", range_string, ",O", rowCurrent, ")<=0")
      conditionalFormatting(wb, 1, rule = rule_string, style = mandatory_input_reject_style, rows = rowCurrent, cols = 15)
      mergeCells(wb, 1, cols = 15:16, rows = rowCurrent)

      #Persistent Comment Field
      addStyle(wb, 1, rows = rowCurrent, cols = 18:27, style = optional_input_style)
      mergeCells(wb, 1, cols = 18:27, rows = rowCurrent) # rat persistent comment merge

      #Warnings
      conditionalFormatting(wb, 1, type = "expression", rule = "==\"Warnings: none\"", style = mandatory_input_accept_style, rows = rowCurrent, cols = 29)
      conditionalFormatting(wb, 1, type = "expression", rule = "!=\"Warnings: none\"", style = warning_style, rows = rowCurrent, cols = 29)
      addStyle(wb, 1, rows = rowCurrent, cols = 29, style = halign_center_style, stack = TRUE) # center the warning text

      #Entire Main rowCurrent
      addStyle(wb, 1, rows = rowCurrent, cols = 1:30, style = rat_header_style, stack = TRUE) # vertically center & wrap the main rowCurrent
      #Set_Height_Main_Row()

      #Retrieve persistent comment if there is one
      comment = rat_archive %>% filter(Rat_ID == ratID) %>% .$Persistent_Comment
      if (is.na(comment)) comment = "[Persistent comment field e.g. week-ahead informal plan for this rat]"

      rat_header_df = data.frame(
        rat_name, #A
        nextrun_text, #B
        "", #C - merge with previous
        "[Tomorrow's Filename]", #D
        "", #E
        experiment_current, #F
        "", #G - merge with previous
        "", #H
        phase_current, #I
        "", #J - merge with previous
        "", #K
        "[Task]", #L,
        "", #M - merge with previous
        "", #N
        "[Detail]", #O,
        "", #P - merge with previous
        "", #Q
        comment, #R
        "", "", "", "", "", #S T U V W
        "", "", "", "", "", #X Y Z AA AB
        "", #AC (Warnings will be filled in dynamically later)
        paste0("#", ratID), #AD, column 30 -- offscreen, but used for readback
        check.names = FALSE, fix.empty.names = FALSE
      )
      writeData(wb, 1, x = rat_header_df, startRow = rowCurrent, colNames = FALSE, rowNames = FALSE)
    }

    Write_Table <- function() {
      Build_Counts <- function() {
        # if experiment_current != Oddball
        # get current date and compare to rat_archive 'HL induced' column's date to determine post-HL or not
        pre_HL = is.na(dplyr::filter(rat_archive, Rat_ID == ratID)$HL_date) #(boolean)
        if (!pre_HL) HL_date = dplyr::filter(rat_archive, Rat_ID == ratID)$HL_date

        # BBN Rxn/TH PreHL Alone
        if (phase_current == "BBN" & task_current %in% c("Rxn", "TH") & pre_HL & detail_current == "Alone") {
          count_df = rat_runs %>%
            tidyr::unnest_wider(assignment) %>%
            dplyr::filter(phase == "BBN" & task %in% c("Rxn", "TH") & detail == "Alone") %>%
            group_by(task) %>%
            summarise(task = unique(task), detail = unique(detail),
                      date = tail(date, 1), n = n(),
                      condition = "baseline",
                      .groups = "drop")
        }

        # BBN Rxn/TH PreHL Mixed
        #TODO Untested as not in current dataset
        if (phase_current == "BBN" & task_current %in% c("Rxn", "TH") & pre_HL & detail_current == "Mixed") {
          count_df = rat_runs %>%
            tidyr::unnest_wider(assignment) %>%
            dplyr::filter(phase == "BBN" & task %in% c("Rxn", "TH") & detail == "Mixed") %>%
            group_by(task) %>%
            summarise(task = unique(task), detail = unique(detail),
                      date = tail(date, 1), n = n(),
                      condition = "baseline",
                      .groups = "drop")
        }

        # BBN Training/Reset PreHL
        #TODO Untested as not in current dataset
        if (phase_current == "BBN" & task_current %in% c("Training", "Reset") & pre_HL) {
          count_df = rat_runs %>%
            tidyr::unnest_wider(assignment) %>%
            dplyr::filter(phase == "BBN" & detail == detail_current) %>%
            group_by(task) %>%
            summarise(task = unique(task), detail = unique(detail),
                      date = tail(date, 1), n = n(),
                      condition = "baseline",
                      .groups = "drop")
        }


        # Gap Detection Training/Reset PreHL
        if (phase_current == "Gap Detection" & task_current %in% c("Training", "Reset") & pre_HL) {
          count_df = rat_runs %>%
            tidyr::unnest_wider(assignment) %>%
            dplyr::filter(phase == "Gap Detection" & detail == detail_current) %>%
            group_by(task) %>%
            summarise(task = unique(task), detail = unique(detail),
                      date = tail(date, 1), n = n(),
                      condition = "baseline",
                      .groups = "drop")
        }


        # Tones Rxn/TH PreHL
        if (phase_current == "Tones" & task_current %in% c("Rxn", "TH") & pre_HL) {
          count_df = rat_runs %>%
            tidyr::unnest_wider(assignment) %>%
            dplyr::filter(phase == "Tones" & task %in% c("Rxn", "TH")) %>%
            group_by(task, detail) %>%
            summarise(task = unique(task), detail = unique(detail),
                      date = tail(date, 1), n = n(),
                      condition = "baseline",
                      .groups = "drop") %>%
            dplyr::arrange(dplyr::desc(detail), task)
        }

        # Tones Training/Reset PreHL
        #TODO Untested as not in current dataset
        if (phase_current == "Tones" & task_current %in% c("Training", "Reset") & pre_HL) {
          count_df = rat_runs %>%
            tidyr::unnest_wider(assignment) %>%
            dplyr::filter(phase == "Tones") %>% # keeping all tasks
            group_by(task) %>%
            summarise(task = unique(task), detail = NA,
                      date = tail(date, 1), n = n(),
                      condition = "baseline",
                      .groups = "drop")
        }

        # BBN/Tones Rxn/TH PostHL
        if (phase_current %in% c("BBN", "Tones") & task_current %in% c("Rxn", "TH") & !pre_HL) {
          df = rat_runs %>%
            mutate(condition = dplyr::if_else(date <= HL_date, "baseline", "post-HL")) %>%
            tidyr::unnest_wider(assignment) %>%
            mutate(duration = dplyr::if_else(
              rapply(.$summary, length, how="unlist") %>% .[seq(7, length(.), 7)] == 1,
              .$summary %>% modify_depth(1, "duration") %>% as.character %>% str_extract("[:digit:]+"),
              "Mixed"
            ))

          # get the duration that was used in today's run
          duration_current = df %>%
            filter(phase == phase_current & task == task_current & detail == detail_current) %>%
            .$duration %>% unique()

          # count number of pre-HL TH runs, and number of post-HL runs by detail
          BBN_counts = df %>%
            dplyr::filter(phase == "BBN") %>%
            dplyr::filter((task == "TH" & condition == "baseline" & detail == detail_current & duration == duration_current)
                          | (condition == "post-HL" & detail == detail_current)) %>%
            group_by(task, condition) %>%
            summarise(task = paste("BBN", unique(task)), detail = unique(detail),
                      date = tail(date, 1), n = n(),
                      condition = unique(condition),
                      .groups = "drop") %>%
            dplyr::arrange(condition, task)

          Tones_counts = df %>%
            dplyr::filter(phase == "Tones") %>%
            dplyr::filter(task %in% c("Rxn", "TH")) %>%
            group_by(task, condition) %>%
            summarise(task = paste("Tones", unique(task)), detail = unique(detail),
                      date = tail(date, 1), n = n(),
                      condition = unique(condition),
                      .groups = "drop") %>%
            dplyr::arrange(condition, task)

          count_df = rbind(BBN_counts, Tones_counts) %>%
            relocate(condition, .after = n)
        }

        # BBN Training/Reset PostHL
        #TODO Partially tested as not in current dataset
        if (phase_current == "BBN" & task_current %in% c("Training", "Reset") & !pre_HL) {
          count_df = rat_runs %>%
            tidyr::unnest_wider(assignment) %>%
            dplyr::filter(phase == "BBN" & date <= HL_date) %>%
            group_by(task) %>%
            summarise(task = unique(task), detail = NA,
                      date = tail(date, 1), n = n(),
                      condition = "post-HL",
                      .groups = "drop")
        }

        # Tones Training/Reset PostHL
        #TODO Partially tested as not in current dataset
        if (phase_current == "Tones" & task_current %in% c("Training", "Reset") & !pre_HL) {
          count_df = rat_runs %>%
            tidyr::unnest_wider(assignment) %>%
            dplyr::filter(phase == "Tones") %>% # keeping all tasks
            group_by(task) %>%
            summarise(task = unique(task), detail = NA,
                      date = tail(date, 1), n = n(),
                      condition = "post-HL",
                      .groups = "drop")
        }

        # Octave
        if (phase_current == "Octave") {
          if (pre_HL) {
            count_df = rat_runs %>%
              tidyr::unnest_wider(assignment) %>%
              dplyr::filter(phase == "Octave") %>% # keeping all tasks
              group_by(task, detail) %>%
              summarise(task = unique(task), detail = unique(detail),
                        date = tail(date, 1), n = n(),
                        condition = "baseline",
                        .groups = "drop")
          } else {
            count_df = rat_runs %>%
              tidyr::unnest_wider(assignment) %>%
              dplyr::filter(phase == "Octave" & date >= HL_date) %>% # keeping all tasks
              group_by(task, detail) %>%
              summarise(task = unique(task), detail = unique(detail),
                        date = tail(date, 1), n = n(),
                        condition = "post-HL",
                        .groups = "drop")
          }
        }

        # Oddball
        # df_basecase = length of most recent streak with task==basecase
        if (experiment_current == "Oddball") {
          df_basecase = rat_runs %>%
            tidyr::unnest_wider(assignment) %>%
            dplyr::filter(phase == phase_current & task == "Base case") %>% # note that this is agnostic of the most recent detail and will return any recent base case streak
            mutate(groupid = data.table::rleid(task, detail) ) %>%
            filter(groupid == suppressWarnings(max(groupid))) %>%
            summarise(task = unique(task), detail = unique(detail),
                      date = tail(date, 1), n = n(),
                      condition = NA,
                      .groups = "drop")

          # df_task = count of today's task
          if (task_current != "Base case") {
            df_task = rat_runs %>%
              tidyr::unnest_wider(assignment) %>%
              dplyr::filter(phase == phase_current & task == task_current) %>%
              summarise(task = unique(task), detail = unique(detail),
                        date = tail(date, 1), n = n(),
                        condition = NA,
                        .groups = "drop")

            # This ensures that the Base case is always at the top
            count_df = rbind(df_basecase, df_task)
          } else count_df = df_basecase
        }

        #format date correctly
        date = count_df$date
        date = paste0(stringr::str_sub(date, 5, 6), "/", stringr::str_sub(date, 7, 8), "/", stringr::str_sub(date, 1, 4))
        count_df$date = date

        count_df = count_df %>% add_column(blank = NA, .after = "n") # col 5 should be blank
        count_df[, (length(count_df) + 1):29] = NA # add columns to reach 29

        return(count_df)
      }




      Build_Table <- function() {
        # Common Columns ----------------------------------------------------------
        columns = c("task", "detail", "date", "file_name", "weight", "trial_count", "hit_percent", "FA_percent", "mean_attempts_per_trial", "threshold", "reaction", "FA_detailed", "warnings_list", "comments", "analysis_type")

        r = rat_runs %>%
          dplyr::filter(map_lgl(assignment, ~ .x$experiment == experiment_current)) %>%
          dplyr::filter(map_lgl(assignment, ~ .x$phase == phase_current)) %>%
          tidyr::unnest_wider(assignment) %>%
          tidyr::unnest_wider(stats) %>%
          tidyr::unnest_wider(summary) %>%
          dplyr::select(all_of(columns)) %>%
          arrange(desc(date), .by_group = F)

        weight_max = max(rat_runs$weight) # Rat_runs not r because we want all history, not just days corresponding to this experiment/phase

        #min_duration = r %>% unnest(reaction) %>% .$`Dur (ms)` %>% unique() %>% min()
        min_duration = r %>% unnest(reaction) %>% dplyr::filter(task == task_current & detail == detail_current) %>% .$`Dur (ms)` %>% unique() %>% min()

        # Needed to deal with the initial training
        analysis_type = r %>% arrange(desc(date)) %>% head(1) %>% .$analysis_type

        # Task-specific RXN column ------------------------------------------------

        if (experiment_current == "Oddball") {
          r = r %>% mutate(reaction1 = reaction) %>%
            unnest(reaction) %>%
            group_by(date) %>%
            mutate(Rxn = mean(Rxn)) %>%
            select(-`Freq (kHz)`, -`Dur (ms)`, -`Inten (dB)`) %>% #TODO check that Frequency is actually go tone positional data
            distinct()
        } else {
          if (phase_current == "Octave") {
            r = r %>% unnest(reaction) %>%
              select(-`Freq (kHz)`, -`Dur (ms)`, -`Inten (dB)`)
          } else {
            # TH and Rxn refer to the task detail not the column
            df_TH_BBN = NULL
            df_TH_tones = NULL
            df_Rxn = NULL

            df_TH_BBN = r %>% unnest(reaction) %>%
              dplyr::filter(task == "TH" & `Dur (ms)` == min_duration & `Freq (kHz)` == 0)  %>%
              group_by(date) %>%
              slice(which.min(`Inten (dB)`))

            intensity = r %>% unnest(reaction) %>%
              dplyr::filter(task == "TH" & `Dur (ms)` == min_duration & `Freq (kHz)` != 0) %>% #select(-threshold, -file_name, - weight, -mean_attempts_per_trial) %>% View
              group_by(date) %>%
              count(`Inten (dB)`) %>% arrange(desc(`Inten (dB)`)) %>% slice(which.max(n)) %>%
              rename(desired_dB = `Inten (dB)`) %>% select(-n)

            df_TH_tones = r %>% unnest(reaction) %>%
              dplyr::filter(task == "TH" & `Dur (ms)` == min_duration & `Freq (kHz)` != 0) %>% #select(-threshold, -file_name, - weight, -mean_attempts_per_trial) %>% View
              right_join(intensity, by = "date") %>%
              filter(`Inten (dB)` == desired_dB) %>%
              group_by(date) %>%
              mutate(Rxn = mean(Rxn)) %>%
              select(-desired_dB)

            # if we have a 60db entry for a date, great
            df_Temp = r %>%
              unnest(reaction) %>%
              dplyr::filter(task != "TH" & `Dur (ms)` == min_duration & `Inten (dB)` == 60) %>% # not equal TH
              group_by(date) %>%
              mutate(Rxn = mean(Rxn))

            # for all dates, take their 55 and 65 entries (stepsize 5 will have potentially all of 55, 60, 65, stepsize 10 will have either 60 or both 55 and 65)
            # take the average Rxn from the 55 and 65 and only keep dates that aren't already in df_Temp
            df_Rxn = r %>%
              unnest(reaction) %>%
              dplyr::filter(task != "TH" & `Dur (ms)` == min_duration & `Inten (dB)` %in% c(55,65)) %>% # not equal TH
              group_by(date) %>%
              summarise(Rxn = mean(Rxn), `Inten (dB)` = mean(`Inten (dB)`), across(), .groups = "drop") %>%
              distinct() %>%
              filter(! date %in% df_Temp$date) %>%
              rbind(df_Temp)

            r = rbind(df_TH_BBN, df_TH_tones, df_Rxn)

            if (analysis_type %in% c("Training - Gap", "Training - BBN")) {
              r = r %>% rename(Dur = `Dur (ms)`, Freq = `Freq (kHz)`) %>% select(-`Inten (dB)`)
            } else {
              r = r %>% select(-`Freq (kHz)`, -`Dur (ms)`, -`Inten (dB)`)
            }

            r = distinct(r)
          }
        }

        r = r %>% select(-analysis_type) %>% relocate(Rxn, .before = mean_attempts_per_trial) %>%
          mutate(Spacer1 = NA)

        # Phase-specific Columns --------------------------------------------------------
        if (analysis_type %in% c("Training - Gap", "Training - BBN")) {
          # Training has no TH
          r = r %>% unnest(threshold) %>% filter(Dur == min_duration) %>% select(-Freq, -Dur) %>%
            group_by(task, detail) %>%
            relocate(Spacer1, .after = mean_attempts_per_trial) %>%
            select(-FA_detailed)
        } else if (phase_current == "BBN") {
          r = r %>% unnest(threshold) %>% filter(Dur == min_duration) %>% select(-Freq, -Dur) %>%
            group_by(task, detail) %>%
            mutate(THrange = paste0(suppressWarnings(min(TH, na.rm = TRUE)) %>% round(digits = 0), "-", suppressWarnings(max(TH, na.rm = TRUE)) %>% round(digits = 0))) %>%
            relocate(THrange, .after = TH) %>%
            relocate(Spacer1, .after = mean_attempts_per_trial) %>%
            select(-FA_detailed)
        } else if (phase_current == "Tones") {

          r = r %>% unnest(threshold) %>%
            filter(Freq != 0 & Dur == min_duration) %>%
            group_by(task, detail, Freq) %>%
            mutate(THrange = paste0(suppressWarnings(min(TH, na.rm = TRUE)) %>% round(digits = 0), "-", suppressWarnings(max(TH, na.rm = TRUE)) %>% round(digits = 0))) %>%
            relocate(THrange, .after = TH) %>%
            gather(variable, value, (TH:THrange)) %>%
            unite(temp, variable, Freq) %>%
            pivot_wider(names_from = temp, values_from = value)

          r = r %>%
            select(-Dur, -FA_detailed) %>%
            relocate(warnings_list, comments, .after = last_col())

          # Adding missing columns without overwriting extant THs and THranges
          df = tibble(TH_4 = NA, TH_8 = NA, TH_16 = NA, TH_32 = NA,
                      THrange_4 = NA, THrange_8 = NA, THrange_16 = NA, THrange_32 = NA)
          r = add_column(r, !!!df[setdiff(names(df), names(r))]) %>%
            relocate(TH_4, TH_8, TH_16, TH_32, THrange_4, THrange_8, THrange_16, THrange_32, .after = Spacer1) %>%
            mutate(Spacer2 = NA) %>%
            relocate(Spacer2, .after = TH_32) %>%
            mutate_at(vars(starts_with("TH_")), as.numeric)

          x = rat_runs %>%
            dplyr::filter(map_lgl(assignment, ~ .x$experiment == experiment_current)) %>%
            dplyr::filter(map_lgl(assignment, ~ .x$phase == phase_current)) %>%
            tidyr::unnest_wider(assignment) %>%
            tidyr::unnest(summary) %>%
            select(task, detail, date, `Freq (kHz)`, dB_min, dB_max) %>%
            group_by(date, task, detail, `Freq (kHz)`) %>% #do(print(.))
            mutate(Spacer3 = NA,
                   Stimrange = paste0(unique(dB_min), "-", unique(dB_max))) %>%
            select(task, detail, date, Spacer3, `Freq (kHz)`, Stimrange)

          r = left_join(r, x, by = c("task", "detail", "date")) %>%
            pivot_wider(names_from = `Freq (kHz)`, values_from = Stimrange)

          # Adding missing columns without overwriting extant THs and THranges
          df = tibble(`4` = NA, `8` = NA, `16` = NA, `32` = NA)
          r = add_column(r, !!!df[setdiff(names(df), names(r))]) %>%
            relocate(`4`, `8`, `16`, `32`, .after = Spacer3)


        } else if (phase_current == "Octave") {
          r = r %>% select(-threshold)

          #TODO NOT MVP convert to 1/12 of octaves based on summary kHz range
          df_discrimination = r %>% filter(task != "Training") %>%
            unnest(FA_detailed) %>%
            group_by(date) %>%
            do(mutate(., Oct = c(1:6), dprime = max(dprime))) %>% # mutate(Oct_position = c(1:6)) %>% print) %>%
            select(-FA, -trials, -`Freq (kHz)`) %>%
            pivot_wider(names_from = Oct, values_from = FA_percent_detailed)

          df_training = r %>% filter(task == "Training") %>% select(-FA_detailed)

          x = rat_runs %>%
            dplyr::filter(map_lgl(assignment, ~ .x$experiment == experiment_current)) %>%
            dplyr::filter(map_lgl(assignment, ~ .x$phase == phase_current)) %>%
            tidyr::unnest_wider(assignment) %>%
            tidyr::unnest_wider(stats) %>%
            select(task, detail, date, dprime) %>%
            dplyr::mutate(date = paste0(stringr::str_sub(date, 5, 6), "/", stringr::str_sub(date, 7, 8), "/", stringr::str_sub(date, 1, 4)))

          df_training = left_join(df_training, x, by = c("task", "detail", "date"))

          r = rbind(df_discrimination, df_training) %>%
            relocate(dprime, .after = Spacer1) %>%
            mutate(Spacer2 = NA) %>% relocate(Spacer2, .after = dprime)

        } else if (experiment_current == "Oddball") {
          r = r %>%
            select(-threshold) %>%
            rename(Rxn_avg = Rxn) %>%
            unnest(c(reaction1, FA_detailed)) %>%
            group_by(date) %>%
            do(filter(., `Inten (dB)` %in% c(min(`Inten (dB)`), median(`Inten (dB)`), max(`Inten (dB)`))) %>%
                 mutate(position = c("early", "mid", "late"))) %>%
            select(-`Freq (kHz)`, -`Dur (ms)`, -`Inten (dB)`, -FA, -trials) %>%
            pivot_wider(names_from = position, values_from = c(Rxn, FA_percent_detailed)) %>%
            mutate(Spacer2 = NA) %>% relocate(Spacer2, .after = Rxn_late)
        }


        averages = r %>%
          dplyr::group_by(task, detail) %>%
          dplyr::summarise_if(.predicate = is.numeric, .funs = mean, na.rm = TRUE) %>%
          dplyr::mutate(date = "Overall", file_name = "Averages", warnings_list = NA, comments = NA) %>%
          dplyr::relocate(date, file_name, .before = weight) %>%
          ungroup() %>%
          mutate_all(~ifelse(is.nan(.), NA, .))

        order = r %>% arrange(desc(date)) %>% group_by(task) %>% do(head(., 1)) %>% arrange(desc(date)) %>% .$task

        r = r %>% arrange(desc(date)) %>% group_by(task) %>%
          do(if (unique(.$task) %in% c("TH", "CNO 3mg/kg")) head(., 10)
             else head(., 3)) %>%
          arrange(match(task, order)) %>%
          dplyr::mutate(date = paste0(stringr::str_sub(date, 5, 6), "/", stringr::str_sub(date, 7, 8), "/", stringr::str_sub(date, 1, 4))) %>%
          mutate(date = as.character(date))

        columns = names(r)

        r = bind_rows(averages, r) %>%
          select(all_of(columns)) %>%
          mutate(weight = (weight - weight_max)/weight_max)

        r[, (length(r) + 1):29] = NA # add columns to reach 29

        r = r %>% relocate(warnings_list, comments, .after = last_col())

        return(as.data.frame(r))  #shed the grouping that prevents rbinding later on
      }

      Build_Table_Key <- function() {
        r = c("", "", "    ", "        ", "     ", "      ", "     ", "    ", "___", "_____")
        r = rbind(r, c("", "", "Date", "Filename", "Wt d%", "Trials", "Hit %", "FA %", "Rxn", "Atmpt")) %>% as.data.frame()
        r = cbind(r, NA) #spacer

        if (phase_current == "BBN" | phase_current == "Gap Detection") {
          r = cbind(r, c("", "TH"))
          r = cbind(r, c("", "{TH}"))
        }
        else if (phase_current == "Tones") {
          r = cbind(r, c("                       TH", "4"))
          r = cbind(r, c("                       TH", "8"))
          r = cbind(r, c("                       TH", "16"))
          r = cbind(r, c("                       TH", "32"))
          r = cbind(r, NA)
          r = cbind(r, c("                {TH} Range", "4"))
          r = cbind(r, c("                {TH} Range", "8"))
          r = cbind(r, c("                {TH} Range", "16"))
          r = cbind(r, c("                {TH} Range", "32"))
          r = cbind(r, NA)
          r = cbind(r, c("              {Stim} Range", "4"))
          r = cbind(r, c("              {Stim} Range", "8"))
          r = cbind(r, c("              {Stim} Range", "16"))
          r = cbind(r, c("              {Stim} Range", "32"))
        }
        else if (phase_current == "Octave") {
          r = cbind(r, c("", "d'"))
          r = cbind(r, NA)
          r = cbind(r, t(data.frame(c("                                             No-Go False Alarm % (by octave steps)") %>% rep_len(12), c(1:12))))
        }
        else if (phase_current == "Tone-BBN" || phase_current == "Tone-Tone") {
          r = cbind(r, c("           Rxn Time", "Early"))
          r = cbind(r, c("           Rxn Time", "Mid"))
          r = cbind(r, c("           Rxn Time", "Late"))
          r = cbind(r, NA)
          r = cbind(r, c("       False Alarm %", "Early"))
          r = cbind(r, c("       False Alarm %", "Mid"))
          r = cbind(r, c("       False Alarm %", "Late"))
          r = cbind(r, NA)
        }
        else {
          stop("ERROR: unrecognized phase: ", phase_current)
        }

        r[, (length(r) + 1):27] = NA # add columns to reach 27
        r = cbind(r, c("", "Warn"))
        r = cbind(r, c("", "Observations"))
        return(r)
      }


      # Write Table Workflow ----------------------------------------------------
      row_table_start = rowCurrent + 1 #save for later
      addStyle(wb, 1, table_header_style, rows = row_table_start, cols = 1:29, gridExpand = TRUE)
      df_table = Build_Table()
      df_key = Build_Table_Key()
      df_counts = Build_Counts()

      # need blank rows inserted into df_table to give space for df_key
      df_blank <- data.frame(matrix(ncol = 29 , nrow = nrow(df_key) + nrow(df_counts)))

      # the obnoxious blank strings are because every column in a table has to have a unique header,
      # and because we want those headers to be blank for all but column A
      names = c("Task (Filter)", "Detail (Filter)", "  ", "   ", "    ", "     ", "      ", "       ", "        ", "         ", "          ", "           ", "            ", "             ", "              ", "               ", "                ", "                 ", "                  ", "                   ", "                    ", "                     ", "                      ", "                       ", "                        ", "                         ", "                          ", "                           ", "                            ")
      colnames(df_table) = names
      colnames(df_blank) = names
      df_table = rbind(df_blank, df_table)

      row_counts = row_table_start + 1
      row_key_start = row_table_start + nrow(df_counts) + 1
      row_key_end = row_table_start + nrow(df_counts) + nrow(df_key)
      row_table_end = row_table_start + nrow(df_table)

      writeDataTable(wb, 1, x = df_table, startRow = row_table_start, colNames = TRUE, rowNames = FALSE, bandedRows = FALSE, tableStyle = "TableStyleMedium18", na.string = "")
      writeData(wb, 1, x = df_counts, startRow = row_counts, colNames = FALSE, rowNames = FALSE)
      writeData(wb, 1, x = df_key, startRow = row_key_start, colNames = FALSE, rowNames = FALSE)

      # style the averages rows
      averages_last_row = row_table_start + max(which(df_table[,3] == "Overall"))
      addStyle(wb, 1, averages_style, rows = row_key_end:averages_last_row, cols = 1:29, gridExpand = TRUE, stack = TRUE)

      # style the 'today' rowCurrent
      today_offset = min(which(df_table[,3] != "Overall"))
      row_today = row_table_start + today_offset
      addStyle(wb, 1, today_style, rows = row_today, cols = 1:29)

      # style the '%' columns
      tk = t(tail(df_key))
      row.names(tk) <- 1:nrow(tk)
      pc1 = tk %>% as.data.frame() %>% filter(str_detect(.[,1], "%")) %>% rownames() %>% as.numeric
      pc2 = tk %>% as.data.frame() %>% filter(str_detect(.[,2], "%")) %>% rownames() %>% as.numeric
      percentage_columns = c(pc1, pc2)
      addStyle(wb, 1, percent_style, rows = row_key_end:row_table_end, cols = percentage_columns, gridExpand = TRUE, stack = TRUE)

      # copy today's warnings
      warns = df_table[today_offset, 28] %>% unlist() %>% stringr::str_c(collapse = "\n")
      if (warns == "") warns = "Warnings: none"
      writeData(wb, 1, x = warns, startRow = rowCurrent, startCol = 29)

      # style the key
      addStyle(wb, 1, key_style, rows = row_key_start:row_key_end, cols = 1:29, gridExpand = TRUE)
      addStyle(wb, 1, key_center, rows = row_key_start:row_key_end, cols = 5:28, gridExpand = TRUE) # don't center observations

      # detect and merge common header cells -- nope, merges can't be done inside a table object, so just style it
      matching_cells = (df_key[1,12] == df_key[1,13] && df_key[1,13] == df_key[1,14])
      if (!is.na(matching_cells) && matching_cells) { # Rxn Time
        deleteData(wb, 1, rows = row_key_start, cols = 13:14, gridExpand = TRUE) # delete all but first
        addStyle(wb, 1, key_merged_style, rows = row_key_start, cols = 12:14)
      }

      matching_cells = (df_key[1,12] == df_key[1,15])
      if (!is.na(matching_cells) && matching_cells) { # TH; rxn above will also fire but we just overwrite what it does
        deleteData(wb, 1, rows = row_key_start, cols = 15)
        addStyle(wb, 1, key_merged_style, rows = row_key_start, cols = 12:15)
      }

      matching_cells = (df_key[1,17] == df_key[1,18] && df_key[1,17] == df_key[1,19] && df_key[1,17] == df_key[1,20])
      if (!is.na(matching_cells) && matching_cells) { # TH Range
        deleteData(wb, 1, rows = row_key_start, cols = 18:20, gridExpand = TRUE)
        addStyle(wb, 1, key_merged_style, rows = row_key_start, cols = 17:20)
      }

      matching_cells = (df_key[1,22] == df_key[1,23] && df_key[1,22] == df_key[1,24] && df_key[1,22] == df_key[1,25])
      if (!is.na(matching_cells) && matching_cells) {  # Stim range
        deleteData(wb, 1, rows = row_key_start, cols = 23:25, gridExpand = TRUE)
        addStyle(wb, 1, key_merged_style, rows = row_key_start, cols = 22:25)
      }

      matching_cells = (df_key[1,16] == df_key[1,17] && df_key[1,17] == df_key[1,18])
      if (!is.na(matching_cells) && matching_cells) {  # FA %
        deleteData(wb, 1, rows = row_key_start, cols = 17:18, gridExpand = TRUE)
        addStyle(wb, 1, key_merged_style, rows = row_key_start, cols = 16:18)
      }

      matching_cells = (df_key[1,14] == df_key[1,25]) # No Go False Alarm % (by octave steps)
      if (!is.na(matching_cells) && matching_cells) {
        deleteData(wb, 1, rows = row_key_start, cols = 15:25, gridExpand = TRUE)
        addStyle(wb, 1, key_merged_style, rows = row_key_start, cols = 14:25)
      }

      # advance rowCurrent for next table
      rowCurrent <<- row_table_end + 2
    }


    # Add Rat To Workbook Workflow --------------------------------------------
    rat_runs <<- run_archive %>% dplyr::filter(rat_ID == ratID) %>% dplyr::arrange(date)
    if (nrow(rat_runs) == 0) {
      warn = paste0("SKIPPED: no runs found for #", ratID)
      warning(paste0(warn, "\n"))
    }
    else {
      run_today <<- rat_runs %>% dplyr::arrange(date) %>% tail(1) # this is really just the most recent run, which could actually be old if a rat didn't run 'today', but that should never happen
      cat(paste0("Processing ", run_today$rat_name, " (#", ratID, ")", "..."))
      experiment_current <<- run_today$assignment[[1]]$experiment
      phase_current <<- run_today$assignment[[1]]$phase
      task_current <<- run_today$assignment[[1]]$task
      detail_current <<- run_today$assignment[[1]]$detail
      Write_Header()
      Write_Table()
      writeLines("Done.")
    }
  }


# Writer Workflow ---------------------------------------------------------
  Define_Styles()
  Setup_Workbook()

  #Add_Rat_To_Workbook(186)
  #OR
  rat_archive %>%
    filter(is.na(end_date)) %>%
    filter(is.na(Assigned_Filename)) %>%
    .$Rat_ID %>%
    lapply(Add_Rat_To_Workbook)

  old_wd = getwd()
  setwd(projects_folder)
  saveWorkbook(wb, "supervisor.xlsx", overwrite = TRUE)
  openXL(paste0(projects_folder, "supervisor.xlsx"))
  setwd(old_wd)
}


# Workflow -----------------------------------------------------------

InitializeWriter()
Workbook_Writer()
rm(list = c("averages_style", "date_style", "experiment_config_df", "halign_center_style",
            "key_center", "key_merged_style", "key_style", "mandatory_input_accept_style",
            "mandatory_input_reject_style", "optional_input_style", "percent_style",
            "rat_header_style", "rat_name_style", "run_today", "table_header_style",
            "today_style", "warning_style", "wb"))

