# User Variables ----------------------------------------------------------

# Date range for filtering
min_date = "2022-01-01"
max_date = "2022-12-01"

# Experiment filtering
experiment = "Fmr1 SD"

# slashes must be either / or \\
modern_matlab_file_location = "Z:/Daily Matlab files"
Box_file_location = "C:/Users/Noelle/Box/Behavior Lab/Projects (Behavior)"

projects_folder = "Z:/Behavior-autoanalysis/"

# Functions ---------------------------------------------------------------
Load_old_file <- function(df) {
  # INVALID not being used
  bad_date = df["Date"]
  group = df["Experiment"]
  rat = df["rat_name"]
  omit = if_else(is.na(df["omit_list"]), "", df["omit_list"])
  
  scientist = df["scientist"]
  weightProblem = if_else(is.na(df["weightProblem"]), "", df["weightProblem"])
  rxnProblem = if_else(is.na(df["rxnProblem"]), "", df["rxnProblem"])
  
  
  # Determine project from the group we are in
  if (group == "Tsc2-LE") project = "Tsc2 Eker"
  else if (group == "Fmr1 SD") project = "Archive/Fmr1 SD"
  else if (group == "Oddball Training") project = "Oddball"
  else project = group
  
  # find location of bad file
  if(bad_date > 20230104) file_location = modern_matlab_file_location
  else file_location = glue("{Box_file_location}/{project}/data")
  
  # Get possible files
  if (group == "Fmr1 SD") {
    files = list.files(paste0(file_location), pattern = paste0(rat, ".*", str_remove_all(bad_date, "-"), ".*.mat$"), 
                       recursive = TRUE, full.names = TRUE)
  } else {
    files = list.files(paste0(file_location, "/", str_remove_all(bad_date, "-")), pattern = paste0(rat, ".*", str_remove_all(bad_date, "-"), ".*.mat$"), 
                       recursive = TRUE, full.names = TRUE)
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
    writeLines(",  Unable to find unique old_excel entries for:")
    writeLines(filter(files, no_auto == TRUE)$file)
    files_checked = discard(files_checked, is_null)
  }
  
  if(length(files_checked) > 0) {
    cat(" attempting to load...\n")
    source(paste0(projects_folder, "main.R"))
    lapply(files_checked, Process_File, old_file = TRUE, ignore_name_check = TRUE, exclude_trials = omit, 
           scientist = scientist, weightProblem = weightProblem, rxnProblem = rxnProblem)
  } else cat(paste("no data found. Can NOT autoload run for",  rat, "on", bad_date, "\n"))
}

Check_if_new <- function(df) {
  entries = filter(run_archive, date == str_remove_all(df["Date"], "-") & rat_name == df["rat_name"])
  r = nrow(entries) == 0
  return(r)
}

# Workflow ----------------------------------------------------------------
InitializeMain()

# Get annotated files
load(paste0(projects_folder, "old_excel_archive.Rdata"))

# add missing columns
old_excel_archive = mutate(old_excel_archive, 
                           omit_list = NA, scientist = NA_character_, weightProblem = NA_character_, rxnProblem = NA_character_)

# filter to manageable date range
missing_entries = filter(old_excel_archive, Date < max_date & Date >= min_date & Experiment == experiment) %>%
  # # these files error instead of loading
  # filter(! (rat_name == "Purple2" & Date == "2022-07-06")) %>%
  # filter(! (rat_name == "Purple1" & Date == "2022-07-06"))
  filter(Date != "2022-07-06")
# Doesn't work need to use the above format for specificity
  # filter(rat_name != "Teal1" & Date != "2022-02-01")
  # filter(rat_name != "Teal6" & Date != "2022-02-10") %>%
  # filter(rat_name != "Teal5" & Date != "2022-02-08")
  # filter(rat_name != "Blue3" & Date != "2022-07-23") %>%
  # filter(rat_name != "Blue4" & Date != "2022-07-01") %>%
  # filter(rat_name != "Blue2" & Date != "2022-06-04")
  # filter(rat_name != "Red2" & Date != "2021-12-21") %>%
  # # list of Nose Out times
  # filter(rat_name != "Teal1" & ! Date %in% c("2022-02-01", "2022-02-04", "2022-02-08")) %>%
  # filter(rat_name != "Teal5" & ! Date %in% c("2022-02-08")) %>%
  # filter(rat_name != "Teal6" & ! Date %in% c("2022-02-10")) %>%
  # # Bad mixed dB files (5 & 10 steps)
  # filter(rat_name != "Teal3" & Date != "2022-03-02") 


# Check if in database
missing_entries =
  missing_entries %>%
  bind_cols(apply(missing_entries, 1, Check_if_new) %>%
              as_tibble_col(column_name = "new")) %>%
  filter(new == TRUE) %>%
  filter(Phase == "Octave")


if (nrow(missing_entries) == 0) {
  writeLines("All deleted entries have already been re-loaded into the system")
  # source(paste0(projects_folder, "admin tools\\check and backup.R"))
} else {
  writeLines(paste0("\nLoading ", nrow(missing_entries), " files."))
  # try to get files and then run through main
  apply(missing_entries, 1, Load_old_file)
  
  # check that load is good
  source(paste0(projects_folder, "admin tools\\check and backup.R"))
}
