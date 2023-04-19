fixer <- function(tableRow, newValue) {
  InitializeMain()
  
  df = run_archive[tableRow,] %>% 
    select(date, rat_name, rat_ID, assignment) %>% unnest_wider(assignment) %>% select(-any_of(comment))
  
  # set value to change
  run_archive[tableRow,]$assignment[[1]]$task = newValue
  
  df = bind_rows(df, run_archive[tableRow,] %>% 
    select(date, rat_name, rat_ID, assignment) %>% unnest_wider(assignment) %>% select(-any_of(comment)))
  
  print(df)
  
  switch(menu(c("Yes", "No"), 
              title="Save modified the entry?", 
              graphics = TRUE),
         # 1 (Yes): Write file
         {
           # Save modification -------------------------------------------------------
           save(run_archive, file = paste0(projects_folder, "run_archive.Rdata"), ascii = TRUE, compress = FALSE)
           outcome = "Fixed"},
         # 2 (No): Abort
         {outcome = "No change"})
  
  # InitializeMain()
  # run_archive[tableRow,] %>% 
  #   select(date, rat_name, rat_ID, assignment) %>% unnest_wider(assignment) %>% select(-comment) %>% print
  
  return(outcome)
}


# filter to rowid
run_archive %>% rowid_to_column %>% filter(rat_name %in% c("RP3")) %>% 
  unnest_wider(assignment) %>%
  unnest_wider(stats) %>% arrange(desc(date)) %>%
  mutate(hit_percent = hit_percent * 100, FA_percent = FA_percent * 100, 
         BG = str_extract(file_name, pattern = "(?<=BG_PKN_).*(?=dB)")) %>% 
  filter(BG == 30) %>% 
  .$rowid

# then lapply
lapply(fixer, newValue = "whatever")