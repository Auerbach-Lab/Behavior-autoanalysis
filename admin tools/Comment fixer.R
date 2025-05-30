# Notes -------------------------------------------------------------------
# Shares code with Assignment fixer. Any bugs will be in both scripts.

comment_fixer <- function(tableRow, commenter, additionalValue_weightProblem, additionalValue_rxnProblem) {
  df = run_archive[tableRow,] %>% 
    select(date, time, rat_name, rat_ID, file_name, weightProblem, rxnProblem)
  
  # set value to change
  if(!is.na(additionalValue_weightProblem)) {
    if(run_archive[tableRow,]$weightProblem == "") new_weight = glue("{commenter}: {additionalValue_weightProblem}")
    else new_weight = glue("{run_archive[tableRow,]$weightProblem}, {commenter}: {additionalValue_weightProblem}")
    
    run_archive[tableRow,]$weightProblem = new_weight
  }
  if(!is.na(additionalValue_rxnProblem)) {
    if(run_archive[tableRow,]$rxnProblem == "") new_rxn = glue("{commenter}: {additionalValue_rxnProblem}")
    else new_rxn = glue("{run_archive[tableRow,]$rxnProblem}, {commenter}: {additionalValue_rxnProblem}")
    
    run_archive[tableRow,]$rxnProblem = new_rxn
  }
  
  df = bind_rows(df, run_archive[tableRow,] %>% 
                   select(date, time, rat_name, rat_ID, file_name, weightProblem, rxnProblem)) %>%
    dplyr::select(-any_of("comment"))
  
  print(df)
  
  switch(menu(c("Yes", "Do NOT change the entry"), graphics = TRUE,
              title = "Save modified entry?"),
         # 1 (Yes): Write file
         {# Push modification -------------------------------------------------------
           if(!is.na(additionalValue_weightProblem)) run_archive[tableRow,]$weightProblem <<- new_weight
           if(!is.na(additionalValue_rxnProblem)) run_archive[tableRow,]$rxnProblem <<- new_rxn
           outcome = "Fixed"},
         # 2 (No): Abort
         {outcome = "No change"})
  
}

InitializeMain()


# Select files to alter ---------------------------------------------------
# use any of the filters that apply to select as sepecific a group as needed

run_archive %>% rowid_to_column %>%
  filter(date %in% c(20250417)) %>%         # filter to a specific date
  # filter(box %in% c(5, 6, 7, 8)) %>%      # Pick based on Box the rat runs in
  # filter(time < 120000) %>%               # select a specific time they ran before
  # filter(rat_name %in% c("Purple3")) %>%  # pick specific rats
  # filter(str_detect(file_name, pattern = "4-32kHz")) %>%    # pick based on file names that contain a string
  
  .$rowid %>%       # get rows in the run_archive table, stop here to check the runs to be changed.
  

  ## Comment fixer =====
  # Apply Comment fix to all entries selected above
  # Leave additionalValue_weightProblem or additionalValue_rxnProblem as NA to remain unchanged.
  lapply(comment_fixer, commenter = "NJ", 
         additionalValue_weightProblem = "System ate original comment. Fed 1 extra pellet because at -15%", 
         additionalValue_rxnProblem = NA)

# Save modification -------------------------------------------------------
save(run_archive, file = paste0(projects_folder, "run_archive.Rdata"), ascii = TRUE, compress = FALSE)
print("Saved")
# Validate and back-up archives
source(paste0(projects_folder, "admin tools\\check and backup.R"))
