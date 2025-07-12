# data loading external file formats
library(R.matlab); library(openxlsx); library(xml2); library(zip);

# data manipulation
library(tidyverse); library(dplyr); library(tidyr); library(rlang); library(stringr); library(purrr); library(data.table)

InitializeReader <- function() {
  options(warn = 1) # we want to display warnings as they occur, so that it's clear which file caused which warnings
  source(paste0(projects_folder, "settings.R"))  # user variables
  rat_archive <<- fread(paste0(projects_folder, "rat_archive.csv"), na.strings = c("N/A","NA"))
}

Workbook_Reader <- function() {
  assignments_df = readWorkbook(xlsxFile = paste0(projects_folder, "supervisor.xlsx"), sheet = 1, cols = c(4, 6, 9, 12, 15, 18, 30), colNames = FALSE,
                                na.strings = c("[Tomorrow's Filename]", "[Experiment]", "[Phase]", "[Task]", "[Detail]", "[Persistent comment field e.g. week-ahead informal plan for this rat]"))
  colnames(assignments_df) = c("Assigned_Filename", "Assigned_Experiment", "Assigned_Phase", "Assigned_Task", "Assigned_Detail", "Persistent_Comment", "Rat_ID")
  assignments_df = assignments_df %>% dplyr::filter(!is.na(Rat_ID)) %>% filter(!is.na(Assigned_Filename))
  assignments_df$Rat_ID = assignments_df$Rat_ID %>% stringr::str_sub(start = 2) %>% as.numeric() # trim off pound sign added for humans, coerce to numeric since that's what rat_archive uses
  #clean white spaces from names as they should never exist
  assignments_df = assignments_df %>% mutate(Assigned_Filename = stringr::str_replace_all(Assigned_Filename, " ", ""))
  
  needed_assignments = rat_archive %>% filter(is.na(end_date) & start_date <= str_remove_all(Sys.Date(), "-")) %>% filter(Assigned_Filename == "") %>% nrow()
  
  
  if (needed_assignments != 0 & nrow(assignments_df) != needed_assignments) {
    warn = paste0("Only ", nrow(assignments_df), " assignments were found. (Expected ", needed_assignments, ")")
    writeLines(paste0(warn, "\n"))
  }
  assignments_mandatory_data = assignments_df %>% dplyr::select(-Persistent_Comment) # comments are allowed to be NA, but nothing else is
  
  if (any(is.na(assignments_mandatory_data))) stop("ERROR: Mandatory values are NA. (Are there non-green cells in the supervisor spreadsheet?)")
  assignments_df$Persistent_Comment = assignments_df$Persistent_Comment %>%
    stringr::str_replace(pattern = "comment field", replacement = NA_character_) # match the (partial) placeholder text for the comment field, replace with NA
  
  rat_archive <<- tryCatch(
    {
      r = dplyr::rows_update(rat_archive, assignments_df, by = "Rat_ID", unmatched = "error")
      tryCatch(
        fwrite(r, paste0(projects_folder, "rat_archive.csv")),
        finally = writeLines(paste0(nrow(assignments_df), " assignments were recorded in `rat_archive.csv`"))
      )
      r # this should NOT be a return, because that will return from the whole function and skip the global assignment above. We do actual returns from the error handlers because those are themselves functions.
    },
    error = function(e) { # this function name is specific to tryCatch and cannot be changed
      warning("A rat id (column AD) from the supervisor spreadsheet was not found in the rat archive (or other error, see below)")
      message("Original error message:")
      warning(e)
      return(rat_archive) #return unmodified
    }
  )
  
  # warn about unassigned rats
  unassigned_rats = rat_archive %>% filter(is.na(end_date) & start_date <= str_remove_all(Sys.Date(), "-")) %>% filter(Assigned_Filename == "")
  if (nrow(unassigned_rats) > 0) {
    warn = warn(paste0(nrow(unassigned_rats), " rats are missing assignments. (", str_flatten_comma(unassigned_rats$Rat_name), ")"))
  }
}

Assignments_Writer <- function() {
  wb = createWorkbook()
  modifyBaseFont(wb, fontSize = 12, fontName = "Calibri")
  addWorksheet(wb, sheetName = "Files Summary")
  
  Find_Stim_Location <- function(Assigned_Filename) {
    
    if(Assigned_Filename == "") {
      return("No assignment")
    } else {
      # Searches the expected (hardcoded) stim file location to find a perfect name match
      locations = list.files("Z:/Stim Files", pattern = paste0("^", Assigned_Filename, ".mat$"), recursive = TRUE, full.names = TRUE) 
    }

    if(length(locations) == 1) return(locations)
    if(length(locations) == 0) return("No match")
    if(length(locations) > 1) {
      print(glue("Multiple matches found for {Assigned_Filename} at:\n {locations}"))
      return("Multiple matches")}
  }
  
  data_table = rat_archive %>% filter(is.na(end_date)) %>%
    arrange(Box) %>%
    # Note that this rowwise adds a small but notable slowdown; for speed up, don't dynamically searching each time
    rowwise() %>%
    mutate(Changed = ifelse(Assigned_Filename == Old_Assigned_Filename, "", "*"),
           stim_location = Find_Stim_Location(Assigned_Filename)) %>%
    select(Rat_name, Box, Assigned_Filename, stim_location) 
  writeDataTable(wb, 1, x = data_table, startRow = 1, colNames = TRUE, rowNames = FALSE, bandedRows = TRUE, tableStyle = "TableStyleMedium2", na.string = "")
  
  # formatting - widths
  options("openxlsx.minWidth" = 6)
  setColWidths(wb, 1, cols = 1:5, widths = "auto")
  
  # formatting - alignment
  center_style <- createStyle(halign = "center")
  addStyle(wb, 1, center_style, rows = 1:50, cols = c(2), gridExpand = TRUE, stack = TRUE)
  
  # formatting - make printable
  pageSetup(wb, 1, top = 0.5, bottom = 0.5, header = 0, footer = 0, fitToHeight = TRUE)
  
  # working directory preservation
  old_wd = getwd()
  setwd(projects_folder)
  
  tryCatch(
    saveWorkbook(wb, "assignments.xlsx", overwrite = TRUE),
    warning = function(warning) if (str_detect(as.character(warning), "Permission denied")) {
      writeLines("ALERT: Assignments.xlsx already open so can't be updated. However, the assignments have been saved to rat_archive.csv")
    } else {
      warning(warning)
    }
  )
  openXL(file = "assignments.xlsx")
  
  # cleanup
  options("openxlsx.minWidth" = 3) # return to default
  setwd(old_wd)
}


# Workflow -----------------------------------------------------------
InitializeReader()
Workbook_Reader()
Assignments_Writer()

