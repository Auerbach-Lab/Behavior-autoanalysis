require(ggplot2)
require(openxlsx)
require(xml2)
require(zip)

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
  colK_blank = ""
  #then cols L-Q depending on task, right? I think NG FA% x Freq is only for particular experiment/phase

  # the obnoxious blank strings are because every column in a table has to have a unique header,
  # and because we want those headers to be blank for all but column A
  df = data.frame(
    "Choose Filter" = colA_phase,
    " " = colB_blank,
    "  " = colC_date,
    "   " = colD_filename,
    "    " = colE_trials,
    "     " = colF_hitpercent,
    "      " = colG_FApercent,
    "       " = colH_dprime,
    "        " = colI_react,
    "         " = colJ_thresh,
    "          " = "", #K
    "           " = "", #L
    "            " = "", #M
    "             " = "", #N
    "              " = "", #O
    "               " = "", #P
    "                " = "", #Q
    "                 " = "", #R
    "                  " = "", #S
    "                   " = "", #T
    "                    " = "", #U
    "                     " = "", #V
    "                      " = "", #W
    "                       " = "", #X
    "                        " = "", #Y
    "                         " = "", #Z
    "                          " = "", #AA
    "                           " = Parse_Warnings(), #AB
    "                            " = run$comments,  #AC
    check.names = FALSE, fix.empty.names = FALSE)
  return(df)
}

Setup_Workbook <- function() {
  wb <- createWorkbook()

  options("openxlsx.borderColour" = "#4F80BD")
  options("openxlsx.borderStyle" = "thin")
  modifyBaseFont(wb, fontSize = 10, fontName = "Calibri")

  addWorksheet(wb, sheetName = "Summary", tabColour = "limegreen")

  setColWidths(wb, 1, cols = "A", widths = 20)
  setColWidths(wb, 1, cols = 2:3, widths = 10)
  setColWidths(wb, 1, cols = "D", widths = 25)
  setColWidths(wb, 1, cols = 5:27, widths = 5)
  setColWidths(wb, 1, cols = 18:27, widths = 5, hidden = TRUE) # hide reserved-for-future columns
  setColWidths(wb, 1, cols = "AB", widths = 18, hidden = TRUE) #
  setColWidths(wb, 1, cols = "AC", widths = 50)

  return(wb)
}

# TODO this should be even smarter and determine if there are assignments for today
# If not, and today is not sunday, then assign for today
# Basically it should advance to the first non-sunday that's unassigned
# But to do any of that I need to know where I'm putting assignment data. Probably rat_archive.
Calculate_Next_Run_Text <- function () {
  nextrun = Sys.Date() + 1
  nextrun_text = "Tomorrow"
  if (nextrun %>% format("%A") == "Sunday") {
    nextrun = nextrun + 1
    nextrun_text = "Monday"
  }
  nextrun_text = paste0(nextrun_text, " - ", nextrun %>% format("%m/%d/%Y"))
  return(nextrun_text)
}


Add_Rat_To_Workbook <- function(wb, row, rat_ID) {
  Set_Height_Main_Row <- function() {
    warning_count = length(run$warnings_list[[1]])
    height_by_count = 13*warning_count+2
    min_height = 28
    height_to_set = max(min_height, height_by_count)
    setRowHeights(wb, 1, rows = row, heights = height_to_set)
  }


  run = run_archive[1,] #TODO more than one run, selected by rat_ID
  rat_name = run$rat_name %>% stringr::str_to_upper(string = .)
  digit_index = str_locate(string = rat_name, pattern = "[:digit:]")[1]
  rat_name = paste0(stringr::str_sub(rat_name, 1, digit_index - 1),
                   " ",
                   stringr::str_sub(rat_name, digit_index, stringr::str_length(rat_name))
  )
  nextrun_text = Calculate_Next_Run_Text()


  df = data.frame(
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


  #Rat Name
  addStyle(wb, 1, rat_name_style, rows = row, cols = 1)

  #Date
  addStyle(wb, 1, date_style, rows = row, cols = 2:3)
  mergeCells(wb, 1, cols = 2:3, rows = row) # date merge

  #Tomorrow's Filename
  conditionalFormatting(wb, 1, type = "contains", rule = "[[", style = mandatory_input_reject_style, rows = row, cols = 4)
  conditionalFormatting(wb, 1, type = "notcontains", rule = "[[", style = mandatory_input_accept_style, rows = row, cols = 4)

  #Experimental Phase
  conditionalFormatting(wb, 1, type = "contains", rule = "[[", style = mandatory_input_reject_style, rows = row, cols = 6:10)
  conditionalFormatting(wb, 1, type = "notcontains", rule = "[[", style = mandatory_input_accept_style, rows = row, cols = 6:10)
  mergeCells(wb, 1, cols = 6:10, rows = row) # experimental phase merge

  #Global Comment Field
  addStyle(wb, 1, rows = row, cols = 12:17, style = optional_input_style) # center the warning text
  mergeCells(wb, 1, cols = 12:17, rows = row) # rat global comment merge

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
  writeData(wb, 1, x = df, startRow = row, colNames = FALSE, rowNames = FALSE)





  # then write the table
  df_table = Build_Row(run)
  table_row = row+1
  writeDataTable(wb, 1, x = df_table, startRow = table_row, colNames = TRUE, rowNames = FALSE, bandedRows = FALSE, tableStyle = "TableStyleMedium18")
  addStyle(wb, 1, table_header_style, rows = table_row, cols = 1:29, gridExpand = TRUE)



  return(wb)
}



# Supervisor Workflow -----------------------------------------------------------
rat_ID = 80 #TODO more than one rat
row = 1 #global, index of the next unwritten row
Define_Styles()
#df = Build_Row()
wb = Setup_Workbook()
wb = Add_Rat_To_Workbook(wb, row)
openXL(wb)
#saveWorkbook(wb, "supervisor.xlsx", overwrite = TRUE)
