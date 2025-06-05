# Notes -------------------------------------------------------------------
# Shares code with Comment fixer. Any bugs will be in both scripts.

# Select files to alter ---------------------------------------------------
# use any of the filters that apply to select as specific a group as needed

rows_to_change =
  run_archive %>% rowid_to_column %>%
  filter(date %in% c(20250605)) %>%         # filter to a specific date
  # filter(box %in% c(5, 6, 7, 8)) %>%      # Pick based on Box the rat runs in
  # filter(time < 120000) %>%               # select a specific time they ran before
  filter(rat_name %in% c("Orange5")) #%>%    # pick specific rats
# filter(str_detect(file_name, pattern = "4-32kHz")) %>%    # pick based on file names that contain a string


# Functions ---------------------------------------------------------------
assignment_fixer <- function(tableRow, newValue_phase, newValue_task, newValue_detail, newValue_invalid) {
  
  df = run_archive[tableRow,] %>% 
    select(date, time, rat_name, rat_ID, file_name, assignment, invalid) %>% unnest_wider(assignment)
  
  # set value to change
  if(!is.na(newValue_phase)) run_archive[tableRow,]$assignment[[1]]$phase = newValue_phase
  if(!is.na(newValue_task)) run_archive[tableRow,]$assignment[[1]]$task = newValue_task
  if(!is.na(newValue_detail)) run_archive[tableRow,]$assignment[[1]]$detail = newValue_detail
  if(newValue_invalid != run_archive[tableRow,]$invalid) {
    run_archive[tableRow,]$invalid = newValue_invalid
    change_invalid = TRUE} else {change_invalid = FALSE}
  
  df = bind_rows(df, run_archive[tableRow,] %>% 
                   select(date, time, rat_name, rat_ID, file_name, assignment, invalid) %>% unnest_wider(assignment)) %>%
    dplyr::select(-any_of("comment"))
  
  print(df)
  
  switch(menu(c("Yes", "Do NOT change the entry"), 
              title="Save modified entry?", 
              graphics = TRUE),
         # 1 (Yes): Write file
         {# Push modification -------------------------------------------------------
           if(!is.na(newValue_phase)) run_archive[tableRow,]$assignment[[1]]$phase <<- newValue_phase
           if(!is.na(newValue_task)) run_archive[tableRow,]$assignment[[1]]$task <<- newValue_task
           if(!is.na(newValue_detail)) run_archive[tableRow,]$assignment[[1]]$detail <<- newValue_detail
           if(change_invalid) run_archive[tableRow,]$invalid <<- newValue_invalid
           outcome = "Fixed"},
         # 2 (No): Abort
         {outcome = "No change"})
}


# Workflow ----------------------------------------------------------------
InitializeMain()

rows_to_change %>%
  .$rowid %>%
  ## Assignment fixer =====
  # Apply Assignment fix to all entries selected above
  # lapply(fixer, newValue_phase = NA, newValue_task = NA, newValue_detail = NA, newValue_invalid = "")
  lapply(assignment_fixer, newValue_phase = NA, 
         newValue_task = "Training", newValue_detail = NA, 
         newValue_invalid = "")

# Save modification -------------------------------------------------------
rows_to_change %>%
  select(date, rat_name, file_name, assignment, invalid) %>%
  unnest_wider(assignment) %>%
  select(date, rat_name, file_name, phase, task, detail, invalid) %>%
  print

switch(select.list(c("Save", "Cancel"), graphics = TRUE,
                   title = "Save modified entry?"),
       # 1 (Yes): Write file
       "Save" = {
         save(run_archive, file = paste0(projects_folder, "run_archive.Rdata"), ascii = TRUE, compress = FALSE)
         print("Saved")
         outcome = "Saved"},
       # 2 (No): Abort
       "Cancel" = {
         print("No changes made to database.")
         outcome = "No change"})

# Validate and back-up archives
source(paste0(projects_folder, "admin tools\\check and backup.R"))
