fixer <- function(tableRow, newValue_phase, newValue_task, newValue_detail, newValue_invalid) {
  
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
  
  switch(menu(c("Yes", "No"), 
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

InitializeMain()

# filter to rowid's needing changing
run_archive %>% rowid_to_column %>%
  # # Old entries marked as Invalid
  # left_join(invalid_list %>% select(rat_name, date, Invalid), 
  #           by = join_by(date, rat_name)) %>%
  # filter(! is.na(Invalid)) %>%
  # New stuff
  filter(date == 20240516) %>%
  filter(rat_name %in% c("Purple3")) %>%
  # filter(str_detect(file_name, pattern = "_BG_PKN_0dB$")) %>%
  # filter(phase != "Amplitude Modulation" & task != "Training") %>%
  # filter(str_detect(file_name, pattern = "(30-90)")) %>%
  # arrange(desc(date))
  
  # then lapply
  .$rowid %>%
  # lapply(fixer, newValue_phase = NA, newValue_task = NA, newValue_detail = NA, newValue_invalid = "")
  lapply(fixer, newValue_phase = NA, newValue_task = "Base case", newValue_detail = NA, newValue_invalid = "")

# Save modification -------------------------------------------------------
save(run_archive, file = paste0(projects_folder, "run_archive.Rdata"), ascii = TRUE, compress = FALSE)
print("Saved")
# Validate and back-up archives
source(paste0(projects_folder, "admin tools\\check and backup.R"))
