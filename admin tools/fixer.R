fixer <- function(tableRow, newValue_phase, newValue_task, newValue_detail, newValue_invalid) {
  
  df = run_archive[tableRow,] %>% 
    select(date, time, rat_name, rat_ID, assignment, invalid) %>% unnest_wider(assignment)
  
  # set value to change
  if(!is.na(newValue_phase)) run_archive[tableRow,]$assignment[[1]]$phase = newValue_phase
  if(!is.na(newValue_task)) run_archive[tableRow,]$assignment[[1]]$task = newValue_task
  if(!is.na(newValue_detail)) run_archive[tableRow,]$assignment[[1]]$detail = newValue_detail
  if(newValue_invalid != run_archive[tableRow,]$invalid) {
    run_archive[tableRow,]$invalid = newValue_invalid
    change_invalid = TRUE} else {change_invalid = FALSE}
  
  df = bind_rows(df, run_archive[tableRow,] %>% 
                   select(date, time, rat_name, rat_ID, assignment, invalid) %>% unnest_wider(assignment))
  
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

# filter to rowid
run_archive %>% rowid_to_column %>%
  filter(rat_name %in% c("Green23")) %>%
  unnest_wider(assignment) %>%
  # filter(phase == "Gap Detection") %>%
  # filter(str_detect(assigned_file_name, pattern = "(25-85|15-75|10-70|20-50|25-55|15-45)")) %>%
  arrange(desc(date)) %>%
  filter(date == 20240215) %>%
  # mutate(BG = str_extract(file_name, pattern = "(?<=BG_PKN_).*(?=dB)")) %>% 
  # filter(task == "Reset")
  
  # then lapply
  .$rowid %>%
  lapply(fixer, newValue_phase = NA, newValue_task = "Rxn", newValue_detail = NA, newValue_invalid = "")

# Save modification -------------------------------------------------------
save(run_archive, file = paste0(projects_folder, "run_archive.Rdata"), ascii = TRUE, compress = FALSE)
print("Saved")
