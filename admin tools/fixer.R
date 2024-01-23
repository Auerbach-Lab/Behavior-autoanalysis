fixer <- function(tableRow, newValue_task, newValue_detail) {
  
  # print(newValue_task)
  # print(newValue_detail)
  
  df = run_archive[tableRow,] %>% 
    select(date, rat_name, rat_ID, assignment) %>% unnest_wider(assignment)
  
  # set value to change
  if(!is.na(newValue_task)) run_archive[tableRow,]$assignment[[1]]$task = newValue_task
  if(!is.na(newValue_detail)) run_archive[tableRow,]$assignment[[1]]$detail = newValue_detail
  
  df = bind_rows(df, run_archive[tableRow,] %>% 
    select(date, rat_name, rat_ID, assignment) %>% unnest_wider(assignment))
  
  print(df)
  
  switch(menu(c("Yes", "No"), 
              title="Save modified entry?", 
              graphics = TRUE),
         # 1 (Yes): Write file
         {# Push modification -------------------------------------------------------
           if(!is.na(newValue_task)) run_archive[tableRow,]$assignment[[1]]$task <<- newValue_task
           if(!is.na(newValue_detail)) run_archive[tableRow,]$assignment[[1]]$detail <<- newValue_detail
           outcome = "Fixed"},
         # 2 (No): Abort
         {outcome = "No change"})
  
  # InitializeMain()
  # run_archive[tableRow,] %>% 
  #   select(date, rat_name, rat_ID, assignment) %>% unnest_wider(assignment) %>% select(-comment) %>% print
  
  return(outcome)
}
InitializeMain()

# filter to rowid
run_archive %>% rowid_to_column %>% 
  filter(rat_name %in% c("Bronze3", "Bronze4")) %>%
  unnest_wider(assignment) %>%
  arrange(desc(date)) %>%
  filter(date %in% c(20240113, 20240112, 20240111, 20240110)) %>%
  # mutate(BG = str_extract(file_name, pattern = "(?<=BG_PKN_).*(?=dB)")) %>% 
  # filter(task == "Reset")

# then lapply
  .$rowid %>%
  lapply(fixer, newValue_task = NA, newValue_detail = "Alone")

# Save modification -------------------------------------------------------
save(run_archive, file = paste0(projects_folder, "run_archive.Rdata"), ascii = TRUE, compress = FALSE)
print("Saved")
