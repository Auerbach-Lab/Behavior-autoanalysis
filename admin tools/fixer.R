fixer <- function(tableRow, newValue) {
  InitializeMain()
  
  df = run_archive[tableRow,] %>% 
    select(date, rat_name, rat_ID, assignment) %>% unnest_wider(assignment) %>% select(-comment)
  
  run_archive[tableRow,]$assignment[[1]]$detail = newValue
  
  df = bind_rows(df, run_archive[tableRow,] %>% 
    select(date, rat_name, rat_ID, assignment) %>% unnest_wider(assignment) %>% select(-comment))
  
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
  
  InitializeMain()
  run_archive[tableRow,] %>% 
    select(date, rat_name, rat_ID, assignment) %>% unnest_wider(assignment) %>% select(-comment) %>% print
  
  return(outcome)
}
