require(ggplot2)
require(openxlsx)
require(xml2)
require(zip)

Define_Styles <- function() {
  rat_name_style <<- createStyle(fontSize = 20, textDecoration = "bold", valign = "center")
  date_style <<- createStyle(valign = "center", halign = "right", wrapText = TRUE)
  mandatory_input_reject_style <<- createStyle(fgFill = "#FFE699", fontColour = "#9C5700", valign = "center", wrapText = TRUE)
  #mandatory_input_accept_style <<-

  table_header_style <<- createStyle(fgFill = "darkgray", fontColour = "white", textDecoration = "bold")
}

Build_Row <- function(run) {
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
    "                           " = run$warnings_list[[1]] %>% unlist() %>% stringr::str_c(collapse = "\t"), #AB
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

  setColWidths(wb, sheet = 1, cols = "A", widths = 20)
  setColWidths(wb, sheet = 1, cols = 2:3, widths = 10)
  setColWidths(wb, sheet = 1, cols = "D", widths = 25)
  setColWidths(wb, sheet = 1, cols = 5:27, widths = 5)
  setColWidths(wb, sheet = 1, cols = 18:28, widths = 5, hidden = TRUE) # hide reserved-for-future columns
  setColWidths(wb, sheet = 1, cols = "AB", widths = 18, hidden = TRUE)
  setColWidths(wb, sheet = 1, cols = "AC", widths = 50)

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
  run = run_archive[2,] #TODO more than one run, selected by rat_ID
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

  writeData(wb, sheet = 1, x = df, startRow = row, colNames = FALSE, rowName = FALSE)
  addStyle(wb, sheet = 1, rat_name_style, rows = row, cols = 1)
  addStyle(wb, sheet = 1, date_style, rows = row, cols = 2:3)
  #addStyle(wb, sheet = 1, rat_header_style, rows = row, cols = 4:29)

  #TODO: Conditional Format
  addStyle(wb, sheet = 1, mandatory_input_reject_style, rows = row, cols = 4)
  addStyle(wb, sheet = 1, mandatory_input_reject_style, rows = row, cols = 6:10)


  mergeCells(wb, sheet = 1, cols = 2:3, rows = row) # date merge
  mergeCells(wb, sheet = 1, cols = 6:10, rows = row) # experimental phase merge
  mergeCells(wb, sheet = 1, cols = 12:17, rows = row) # rat global comment merge



  # then write the table
  df_table = Build_Row(run_archive[2,])
  table_row = row+1
  writeDataTable(wb, sheet = 1, x = df_table, startRow = table_row, colNames = TRUE, rowNames = FALSE, bandedRows = FALSE, tableStyle = "TableStyleMedium18")
  addStyle(wb, sheet = 1, table_header_style, rows = table_row, cols = 1:29, gridExpand = TRUE)

  #TODO change AB3 hardcoded to tablerow etc
  v = c("SUBSTITUTE(AB3, \"	\", \"\n\")") # formula needed to turn list of warnings into 1-cell wrapping with line breaks
  writeFormula(wb, sheet = 1, x = v, startRow = row, startCol = 29)

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
