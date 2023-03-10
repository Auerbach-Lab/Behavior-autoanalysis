Find_Issues_in_Archives <- function(group) {
  cat(glue("Checking {group}..."))
  # Load file
  trials_archive = fread(paste0(projects_folder, group, "_archive.csv.gz"))
  
  # Get list of entered trials
  UUIDs_in_trials = trials_archive$UUID %>% unique()
  # Get list of entered runs
  InitializeMain()
  UUIDs_in_runs = run_archive %>% unnest_wider(assignment) %>% filter(experiment == group) %>% .$UUID %>% unique()
  trial_counts = run_archive %>% unnest_wider(assignment) %>% filter(experiment == group) %>% transmute(trials = map_dbl(stats, ~.$trial_count), UUID = UUID)
  trials_total = sum(trial_counts$trials)
  
  if(nrow(trials_archive) != trials_total) {
    cat(glue(" Issue with archive: off by {abs(trials_total-nrow(trials_archive))}"))
    
    trial_count_comparison = trials_archive %>% group_by(UUID) %>% summarise(rows = length(UUID)) %>%
      left_join(trial_counts, by = "UUID") %>%
      mutate(Issue = rows != trials)
    
    Bad_UUIDs = filter(trial_count_comparison, Issue == TRUE) %>% left_join(run_archive %>% select(date, rat_name, rat_ID, file_name, UUID), by = "UUID")
    
    if(nrow(trial_count_comparison) == 0) {
      writeLines(glue("ISSUE IN {group}_archive.csv.gz"))
    } else {
      glue_data(Bad_UUIDs)
      
      
      # De-duplicating ----------------------------------------------------------
      
      # Remove
      trials_archive_dedup = unique(trials_archive)
      
      if(nrow(trials_archive_dedup) != nrow(trials_archive)) {
        writeLines("Duplicate entries found in trials archive")
        switch (menu(c("Yes", "No"), 
                     title = glue("Write out new {group}_archive?
                                    Removing {nrow(trials_archive)-nrow(trials_archive_dedup)} trials. Number of trials will now be {nrow(trials_archive_dedup)} instead of {nrow(trials_archive)}"), 
                     graphics = FALSE),
                # 1 (Yess): Write file
                fwrite(trials_archive_dedup, paste0(projects_folder, group, "_archive.csv.gz")),
                # 2 (No): Abort
                writeLines("Aborting. No changes made")
        )
        
        # Finding missing runs ----------------------------------------------------
        # Find what is missing
        UUIDs_missing = keep(UUIDs_in_runs, ~ ! . %in% UUIDs_in_trials)
        UUIDs_without_runs = keep(UUIDs_in_trials, ~ ! . %in% UUIDs_in_runs)
        
        if(is_empty(UUIDs_without_runs)) {
          writeLines(glue("\tNo extra data for {group}"))
          rm(UUIDs_without_runs)
        } else {
          writeLines("Removing orphan trial data... ")
          trials_archive = filter(trials_archive, ! UUID %in% UUIDs_without_runs)
          fwrite(trials_archive, file = paste0(projects_folder, group, "_archive.csv.gz"))
          cat("Done.")
          rm(UUIDs_without_runs)
        }
        
        # Write out list to load
        temp = run_archive %>% filter(UUID %in% UUIDs_missing) %>% 
          arrange(date) %>%
          select(date, rat_name, rat_ID, omit_list, assignment) %>% 
          unnest_wider(assignment) %>% 
          unnest(omit_list) %>%
          mutate(Dataset = group)
        
        if(nrow(temp) >= 1) {
          writeLines(glue("\tMissing data for {group}"))
          fwrite(temp, "missing.csv")
        } else { writeLines(glue("\tNo missing data for {group}")) }
      }
    }
    

  } else cat(" all good\n")
}

list.files(path = projects_folder, pattern = "^.*_archive.csv.gz$") %>% paste0(projects_folder, .) %>%
  data.frame(file = .) %>% mutate(experiment = str_extract(file, pattern = '(?<=s/).*(?=_a)')) %>% .$experiment %>%
  lapply(Find_Issues_in_Archives)