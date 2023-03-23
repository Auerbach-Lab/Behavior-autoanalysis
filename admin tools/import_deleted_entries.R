# User Variables ----------------------------------------------------------

# slashes must be either / or \\
modern_matlab_file_location = "Z:/Daily Matlab files"
Box_file_location = "C:/Users/Noelle/Box/Behavior Lab/Projects (Behavior)"


# Functions ---------------------------------------------------------------
Load_old_file <- function(df) {
  bad_date = df["date"]
  group = df["experiment"]
  rat = df["rat_name"]
  omit = if_else(is.na(df["omit_list"]), "", df["omit_list"])
  
  # Determine project from the group we are in
  if (group == "Tsc2-LE") project = "Tsc2 Eker"
  else if (group == "Fmr1 SD") project = "Archive/Fmr1 SD"
  else project = group
  
  # find location of bad file
  if(bad_date > 20230104) file_location = modern_matlab_file_location
  else file_location = glue("{Box_file_location}/{project}/data")
  
  # Get possible files
  if (group == "Fmr1 SD") {
    files = list.files(paste0(file_location), pattern = paste0(rat, ".*", bad_date, ".*.mat$"), recursive = TRUE, full.names = TRUE)
  } else {
    files = list.files(paste0(file_location, "/", bad_date), pattern = paste0(rat, ".*", bad_date, ".*.mat$"), recursive = TRUE, full.names = TRUE)
  }
  cat(glue("
                        Found {length(files)} possible files "))
  
  # Check for old assignments
  old_assignment_check <- function(file) {
    old_data = old_excel_archive %>% dplyr::filter(Date == lubridate::ymd(bad_date) & rat_name == rat)
    
    if(nrow(old_data) == 1) {
      cat("with data ")
      return(file)
    } else {
      return(NULL)
    }
    
  }
  
  files_checked = lapply(files, old_assignment_check)
  files = tibble(file = files, no_auto = lapply(files_checked, is_null) %>% as.logical)
  
  # ID files that can't be auto-delt with because of having multiple or no matches
  if(any(files$no_auto)) {
    writeLines(",  Unable to find old_excel entries for:")
    writeLines(filter(files, no_auto == TRUE)$file)
    files_checked = discard(files_checked, is_null)
  }
  
  if(length(files_checked) > 0) {
    cat(" attempting to load...\n")
    source(paste0(projects_folder, "main.R"))
    lapply(files_checked, Process_File, old_file = TRUE, ignore_name_check = TRUE, exclude_trials = omit)
  } else cat(glue("no data found. Can NOT autoload run for
                          {UUID}"))
}


# Workflow ----------------------------------------------------------------


# Append repaired entry to old_excel_archive ------------------------------
# load old_excel_archive
load(paste0(projects_folder, "old_excel_archive.Rdata"))
deleted_entries = fread(paste0(projects_folder, "deleted_entries.csv"))

# check if run currently found in system
deleted_entries_check = 
  deleted_entries %>% 
  # create new column of UUID from the current run_archive
  bind_cols(apply(deleted_entries, 1,
                  function(x) filter(run_archive, date == x["date"] & rat_ID == as.numeric(x["rat_ID"]))$UUID) %>%
              as_tibble_col(column_name = "UUIDnew")) %>%
  # convert no results to NA
  mutate(UUIDnew = if_else(UUIDnew == "character(0)", NA_character_, paste(UUIDnew)))

# get entries that need to put into the system
missing_entries = filter(deleted_entries_check, is.na(UUIDnew)) %>% select(-comment)

# append these runs to the old excel
old_excel_archive = 
  bind_rows(old_excel_archive, missing_entries %>%
              select(date, assigned_file_name, weight, comments, experiment, phase, task, detail, rat_name) %>%
              # fix date to a date object
              mutate(Date = lubridate::ymd(date), Invalid = "") %>% select(-date) %>%
              rename(Filename = assigned_file_name,
                     Weight = weight, "Comments/Observations" = comments,
                     Experiment = experiment, Phase = phase, Task = task, Detail = detail)) %>%
  unique


if (nrow(missing_entries) == 0) {
  writeLines("All deleted entries have already been re-loaded into the system")
} else {
  writeLines(paste0("\nLoading ", nrow(missing_entries), " files."))
  # try to get files and then run through main
  apply(missing_entries, 1, Load_old_file)
  
  # check that everything loaded
  InitializeMain()
  
  deleted_entries_final_check = 
    deleted_entries %>% 
    # create new column of UUID from the current run_archive
    bind_cols(apply(deleted_entries, 1,
                    function(x) filter(run_archive, date == x["date"] & rat_ID == as.numeric(x["rat_ID"]))$UUID) %>%
                as_tibble_col(column_name = "UUIDnew")) %>%
    # convert no results to NA
    mutate(UUIDnew = if_else(UUIDnew == "character(0)", NA_character_, paste(UUIDnew)))
  
  # get entries that need to put into the system
  failed_entries = filter(deleted_entries_final_check, is.na(UUIDnew))
  
  writeLines("\n")
  if(nrow(failed_entries) == 0) { writeLines("Done attempting to load missing runs")
  } else {
    writeLines("Some runs failed to load data:")
    print(failed_entries)
  }
}