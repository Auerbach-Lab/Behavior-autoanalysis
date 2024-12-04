# User Variables ----------------------------------------------------------

dates = c(20241204)

# slashes must be either / or \\
modern_matlab_file_location = "Z:/Daily Matlab files"


# Functions ---------------------------------------------------------------

Loading_files <- function(df) {
  Process_File(df["file"], old_file = TRUE, ignore_name_check = TRUE, 
               exclude_trials = df["omit_list"], 
               scientist = df["scientist"], weightProblem = df["weightProblem"], rxnProblem = df["rxnProblem"])
}

# Workflow ----------------------------------------------------------------
InitializeMain()
source(paste0(projects_folder, "main.R"))

# Entered runs
runs_entered = 
  run_archive %>%
  filter(date %in% dates)
  
  
# Get matlab files
matlab_files = list.files(path = paste0(modern_matlab_file_location, "/", dates), pattern = ".mat$", recursive = TRUE, full.names = TRUE)

matlab_files_table = matlab_files %>% as_tibble %>% rename(file = value) %>% 
  mutate(rat = str_extract(file, pattern = glue("{dates}/.+?_")) %>% str_remove(glue("{dates}/")) %>% str_remove("_"),
         date_time = str_extract(file, pattern = glue("{dates}-[:digit:]+?_")) %>% str_remove_all("_"),
         date = str_extract(date_time, pattern = "^[:digit:]+") %>% as.numeric(),
         time = str_extract(date_time, pattern = "[:digit:]+?$") %>% as.numeric())

# check if run currently found in system
files_not_entered =
  matlab_files_table %>%
  left_join(runs_entered %>%
              select(date, time, rat_name, UUID),
            by = join_by(rat == rat_name, date, time)) %>%
  filter(is.na(UUID))

# Prep for entry
files_to_enter = 
  files_not_entered %>%
  left_join(runs_entered %>%
              select(date, rat_name, rat_ID, weight, assignment, comments, weightProblem) %>%
              unnest_wider(assignment),
            by = join_by(rat == rat_name, date)) %>%
  rename(rat_name = rat) %>%
  mutate(scientist = "Autoload",
         rxnProblem = "", omit_list = "")

  
# append these runs to the old excel
old_excel_archive = 
bind_rows(
  files_to_enter %>%
    select(date, assigned_file_name, weight, comments, experiment, phase, task, detail, comment, rat_name, rat_ID,
           scientist, weightProblem, rxnProblem, omit_list) %>%
    # fix date to a date object
    mutate(Date = lubridate::ymd(date), Invalid = "") %>% select(-date) %>%
    rename(Filename = assigned_file_name,
           Weight = weight, "Comments/Observations" = comments,
           Experiment = experiment, Phase = phase, Task = task, Detail = detail)) %>%
unique


if(nrow(files_to_enter) > 0) {
  writeLines(glue("Loading {nrow(files_to_enter)} files...\n"))
  apply(files_to_enter, 1, Loading_files)
} else cat(glue("No extra files found. Can NOT autoload runs on {dates}"), "\n")
  
