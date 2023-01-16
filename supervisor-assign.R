# data loading external file formats
library(R.matlab); library(openxlsx); library(xml2); library(zip);

# data manipulation
library(tidyverse); library(dplyr); library(tidyr); library(rlang); library(stringr); library(purrr); library(data.table)

InitializeReader <- function() {
  options(warn=1) # we want to display warnings as they occur, so that it's clear which file caused which warnings
  source("A:/Coding/Behavior-autoanalysis/settings.R")  # hardcoded user variables
  rat_archive <<- read.csv(paste0(user_settings$projects_folder, "rat_archive.csv"), na.strings = c("N/A","NA"))
}

Workbook_Reader <- function() {
  assignments_df = readWorkbook(xlsxFile = "supervisor.xlsx", sheet = 1, cols = c(4, 6, 9, 12, 15, 18, 30), colNames = FALSE)
  colnames(assignments_df) = c("Assigned_Filename", "Assigned_Experiment", "Assigned_Phase", "Assigned_Task", "Assigned_Detail", "Persistent_Comment", "Rat_ID")
  assignments_df = assignments_df %>% dplyr::filter(!is.na(Rat_ID))
  assignments_df$Rat_ID = assignments_df$Rat_ID %>% stringr::str_sub(start = 2) %>% as.numeric() # trim off pound sign added for humans, coerce to numeric since that's what rat_archive uses
  if (nrow(assignments_df) != user_settings$runs_per_day) {
    warn = paste0("Only ", nrow(assignments_df), " assignments were found. (Expected ", user_settings$runs_per_day, ")")
    warning(paste0(warn, "\n"))
  }
  assignments_mandatory_data = assignments_df %>% dplyr::select(-Persistent_Comment) # comments are allowed to be NA, but nothing else is
  if (any(is.na(assignments_mandatory_data))) stop("ERROR: Mandatory values are NA. (Are there non-green cells in the supervisor spreadsheet?)")
  assignments_df$Persistent_Comment = assignments_df$Persistent_Comment %>%
    stringr::str_replace(pattern = "comment field", replacement = NA_character_) # match the (partial) placeholder text for the comment field, replace with NA

  rat_archive <<- tryCatch(
    {
      r = dplyr::rows_update(rat_archive, assignments_df, by = "Rat_ID", unmatched = "error")
      tryCatch(
        write.csv(rat_archive, paste0(user_settings$projects_folder, "rat_archive.csv"), row.names = FALSE),
        finally = writeLines(paste0(nrow(assignments_df), " assignments were recorded in `rat_archive.csv`"))
      )
      return(r)
    },
    error = function(e) { # this function name is specific to tryCatch and cannot be changed
      warning("A rat id (column AD) from the supervisor spreadsheet was not found in the rat archive (or other error, see below)")
      message("Original error message:")
      warning(e)
      return(rat_archive) #return unmodified
    }
  )
}

# Workflow -----------------------------------------------------------

Workbook_Reader()