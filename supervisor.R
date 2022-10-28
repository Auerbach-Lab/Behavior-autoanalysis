require(ggplot2)
require(openxlsx)
require(xml2)
require(zip)
require(dplyr)
require(stringr)

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
  setColWidths(wb, 1, cols = "AB", widths = 18, hidden = FALSE)
  setColWidths(wb, 1, cols = "AC", widths = 50)

  return(wb)
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
  return(df)
}

Add_Rat_To_Workbook <- function(row, rat_ID) {
  Build_Header <- function() {
    Calculate_Next_Run_Text <- function () {
      # TODO this should be even smarter and determine if there are assignments for today
      # If not, and today is not sunday, then assign for today
      # Basically it should advance to the first non-sunday that's unassigned
      # But to do any of that I need to know where I'm putting assignment data. Probably rat_archive.
      nextrun = Sys.Date() + 1
      nextrun_text = "Tomorrow"
      if (nextrun %>% format("%A") == "Sunday") {
        nextrun = nextrun + 1
        nextrun_text = "`Monday`"
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

    # Header Workflow ---------------------------------------------------------

    #Rat Name
    rat_name = run$rat_name %>% stringr::str_to_upper(string = .)
    digit_index = str_locate(string = rat_name, pattern = "[:digit:]")[1]
    rat_name = paste0(stringr::str_sub(rat_name, 1, digit_index - 1),
                      " ",
                      stringr::str_sub(rat_name, digit_index, stringr::str_length(rat_name))
    )
    addStyle(wb, 1, rat_name_style, rows = row, cols = 1)

    #Date
    addStyle(wb, 1, date_style, rows = row, cols = 2:3)
    mergeCells(wb, 1, cols = 2:3, rows = row) # date merge
    nextrun_text = Calculate_Next_Run_Text()

    #Tomorrow's Filename
    conditionalFormatting(wb, 1, type = "contains", rule = "[[", style = mandatory_input_reject_style, rows = row, cols = 4)
    conditionalFormatting(wb, 1, type = "notcontains", rule = "[[", style = mandatory_input_accept_style, rows = row, cols = 4)

    #Experimental Phase
    conditionalFormatting(wb, 1, type = "contains", rule = "[[", style = mandatory_input_reject_style, rows = row, cols = 6)
    conditionalFormatting(wb, 1, type = "notcontains", rule = "[[", style = mandatory_input_accept_style, rows = row, cols = 6)

    addWorksheet(wb, "Sheet 2")
    writeData(wb, sheet = 2, x = c("Base case", "Rotating", "Background", "Catch trials", "Uneven odds"))
    dataValidation(wb, 1, rows = row, cols = 6, type = "list", value = "'Sheet 2'!$A$1:$A$5", operator = "ignored for lists")

    mergeCells(wb, 1, cols = 6:7, rows = row) # experimental phase merge

    #Global Comment Field
    addStyle(wb, 1, rows = row, cols = 12:27, style = optional_input_style) # center the warning text
    mergeCells(wb, 1, cols = 12:27, rows = row) # rat global comment merge

    #Warnings
    conditionalFormatting(wb, 1, type = "expression", rule = "==\"Warnings: none\"", style = mandatory_input_accept_style, rows = row, cols = 28:29)
    conditionalFormatting(wb, 1, type = "expression", rule = "!=\"Warnings: none\"", style = warning_style, rows = row, cols = 28:29)
    addStyle(wb, 1, rows = row, cols = 28:29, style = halign_center_style, stack = TRUE) # center the warning text

    # openxlsx has trouble writing linebreaks to a single cell
    # so instead, above in Parse_Warnings we write them tab-separated
    # This excel formula then reads the tab-sep cell and formats it with linebreaks, as a workaround
    v = c("IF(LEN(AB3)=0,\"Warnings: none\", SUBSTITUTE(AB3, \"	\", \"\n\"))") #TODO change AB3 hardcoded to tablerow etc
    writeFormula(wb, 1, x = v, startRow = row, startCol = 28)
    mergeCells(wb, 1, cols = 28:29, rows = row) # header warning merge

    #Entire Main Row
    addStyle(wb, 1, rows = row, cols = 1:29, style = rat_header_style, stack = TRUE) # vertically center & wrap the main row
    Set_Height_Main_Row()

    rat_header_df = data.frame(
      rat_name, #A
      nextrun_text, #B
      "", #C - merge with previous
      "[[Tomorrow's Filename]]", #D
      "", #E
      "[[Experimental Phase]]", #TODO is there any way to make this a dropdown of options? Yes, openxlsx supports data validation
      "", #G - merge with previous
      "", #H - merge with previous
      "", #I - merge with previous
      "", #J - merge with previous
      "", #K,
      "[[Global comment field including e.g. week-ahead informal plan for this rat]]", #L
      "", "", "", "", "", #M N O P Q
      "", "", "", "", "", #R S T U V
      "", "", "", "", "", #W X Y Z AA
      "", #AB
      check.names = FALSE, fix.empty.names = FALSE
    )

    writeData(wb, 1, x = rat_header_df, startRow = row, colNames = FALSE, rowNames = FALSE)
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

    Build_Counts <- function() {
      # NOELLE
      # get all phases (slice run_archive by rat_ID and rat_archive$rat_ID$current_experiment? OR grab experiment and then associated phases from some experiment_archive)
      # then get obs counts per phase
      # then append most recent date of phase
      # then grab today's observations and put them in place
      # then fold into df_table
    }

    Add_Phase_to_Table <- function() {
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
    df_table = Build_Row(run)
    df_table = Finalize_Table()
    writeDataTable(wb, 1, x = df_table, startRow = row_table_start, colNames = TRUE, rowNames = FALSE, bandedRows = FALSE, tableStyle = "TableStyleMedium18")

  }


  # NOTE we aren't doing anything special for handling multiple runs in the same day - they have a warning and we display recent run history, that's enough
  run = run_archive[1,] #TODO more than one run, selected by rat_ID

  # write the header
  Build_Header()

  # then write the table
  Build_Table()
}



# Supervisor Workflow -----------------------------------------------------------
rat_ID = 80 #TODO more than one rat
row = 1 #global, index of the next unwritten row
Define_Styles()
#df = Build_Row()
Setup_Workbook()
Add_Rat_To_Workbook(row, rat_ID)
openXL(wb)
#saveWorkbook(wb, "supervisor.xlsx", overwrite = TRUE)
