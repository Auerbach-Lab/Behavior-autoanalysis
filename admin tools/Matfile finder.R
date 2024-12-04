# User Variables ----------------------------------------------------------

file_list = 
  run_archive %>% 
  select(rat_name, rat_ID, date, file_name, assignment, invalid) %>%
  left_join(select(rat_archive, c(Rat_ID, Genotype)), by = c("rat_ID" = "Rat_ID")) %>%
  filter(str_detect(Genotype, pattern = "Fmr1-LE")) %>%
  unnest_wider(assignment) %>%
  filter(experiment %in% c("Oddball") & task %in% c("Catch trials") & invalid != TRUE) %>% 
  arrange(desc(date)) %>%
  select(-comment, -detail, -assigned_file_name, -invalid)

# slashes must be either / or \\
destination_folder = "C:/Users/Noelle/Box/Behavior Lab/Shared/Ben/Oddball catch trials"

# Should NOT Change
modern_matlab_file_location = "Z:/Daily Matlab files"
Box_file_location = "C:/Users/Noelle/Box/Behavior Lab/Projects (Behavior)"


# Functions ---------------------------------------------------------------
Find_file <- function(df) {
  # print(df)
  
  date = df["date"]
  group = df["experiment"]
  rat = df["rat_name"]
  file_name = df["file_name"]
  
  
  # Determine project from the group we are in
  if (group == "Tsc2-LE" & date < 20230104) project = "Tsc2 Eker"
  else if (group == "Fmr1 SD" & date < 20230104) project = "Archive/Fmr1 SD"
  else if (group == "Oddball Training" & date < 20230104) project = "Oddball"
  else project = group
  
  # find location of bad file
  if(date > 20230104) file_location = modern_matlab_file_location
  else file_location = glue("{Box_file_location}/{project}/data")
  
  # Get possible files
  if (group == "Fmr1 SD") {
    files = list.files(paste0(file_location), pattern = paste0(rat, ".*", date, ".*", ".mat$"), recursive = TRUE, full.names = TRUE)
  } else {
    files = list.files(paste0(file_location, "/", date), pattern = glue("{rat}_{file_name}_{date}.*.mat$"), recursive = TRUE, full.names = TRUE)
  }
  
  writeLines(glue("Copying {file_name} from {date}"))
  
  ## Copy files to new location
  file.copy(from = files, to = destination_folder, 
            overwrite = TRUE, recursive = FALSE, 
            copy.mode = TRUE)
  
  # return(files)
  }

# Workflow ----------------------------------------------------------------
InitializeMain()

writeLines(paste0("\nFinding ", nrow(file_list), " files."))
# try to get files and then run through main
# apply(missing_entries, 1, print)
apply(file_list, 1, Find_file)
