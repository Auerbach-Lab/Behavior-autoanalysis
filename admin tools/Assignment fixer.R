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

comment_fixer <- function(tableRow, commenter, additionalValue_weightProblem, additionalValue_rxnProblem) {
  df = run_archive[tableRow,] %>% 
    select(date, time, rat_name, rat_ID, file_name, weightProblem, rxnProblem)
  
  # set value to change
  if(!is.na(additionalValue_weightProblem)) run_archive[tableRow,]$weightProblem = 
      glue("{run_archive[tableRow,]$weightProblem}, {commenter}: {additionalValue_weightProblem}")
  if(!is.na(additionalValue_rxnProblem)) run_archive[tableRow,]$rxnProblem = 
      glue("{run_archive[tableRow,]$rxnProblem}, {commenter}: {additionalValue_rxnProblem}")
  
  df = bind_rows(df, run_archive[tableRow,] %>% 
                   select(date, time, rat_name, rat_ID, file_name, weightProblem, rxnProblem)) %>%
    dplyr::select(-any_of("comment"))
  
  print(df)
  
  switch(menu(c("Yes", "Do NOT change the entry"), graphics = TRUE,
              title = "Save modified entry?"),
         # 1 (Yes): Write file
         {# Push modification -------------------------------------------------------
           if(!is.na(additionalValue_weightProblem)) run_archive[tableRow,]$weightProblem <<- glue("{run_archive[tableRow,]$weightProblem}, {commenter}: {additionalValue_weightProblem}")
           if(!is.na(additionalValue_rxnProblem)) run_archive[tableRow,]$rxnProblem <<- glue("{run_archive[tableRow,]$rxnProblem}, {commenter}: {additionalValue_rxnProblem}")
           outcome = "Fixed"},
         # 2 (No): Abort
         {outcome = "No change"})
  
}

InitializeMain()


# Select files to alter ---------------------------------------------------

run_archive %>% rowid_to_column %>%
  # # Old entries marked as Invalid
  # left_join(invalid_list %>% select(rat_name, date, Invalid), 
  #           by = join_by(date, rat_name)) %>%
  # filter(! is.na(Invalid)) %>%
  filter(date >= 20240528) %>% # filter to a specific date
  filter(rat_name %in% c("Green1")) %>% # pick specific rats
  # filter(str_detect(file_name, pattern = "_BG_PKN_0dB$")) %>% # pick specific files
  
  .$rowid %>% #get rows in the run_archive table
  
  
  ## Assignment fixer =====
  # Apply Assignment fix to all entries selected above
  # lapply(fixer, newValue_phase = NA, newValue_task = NA, newValue_detail = NA, newValue_invalid = "")
  lapply(assignment_fixer, newValue_phase = NA, 
         newValue_task = NA, newValue_detail = NA, 
         newValue_invalid = "TRUE")

  ## Comment fixer =====
  # # Apply Comment fix to all entries selected above
  # # lapply(fixer, newValue_phase = NA, newValue_task = NA, newValue_detail = NA, newValue_invalid = "")
  # lapply(comment_fixer, commenter = "NJ", additionalValue_weightProblem = "System ate original comment. Fed 1 extra pellet because at -15%", additionalValue_rxnProblem = NA)

# Save modification -------------------------------------------------------
save(run_archive, file = paste0(projects_folder, "run_archive.Rdata"), ascii = TRUE, compress = FALSE)
print("Saved")
# Validate and back-up archives
source(paste0(projects_folder, "admin tools\\check and backup.R"))
