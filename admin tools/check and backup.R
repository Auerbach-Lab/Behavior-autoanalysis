
# User Variables ----------------------------------------------------------

# slashes must be either / or \\
modern_matlab_file_location = "Z:/Daily Matlab files"
Box_file_location = "C:/Users/Noelle/Box/Behavior Lab/Projects (Behavior)"


# Functions ---------------------------------------------------------------
source(paste0(projects_folder, "/admin tools/clean archives.R"))

Find_Issues_in_Archives <- function(group) {
  cat(glue("     Checking {group}..."))
  # Load file
  trials_archive = fread(paste0(projects_folder, group, "_archive.csv.gz"))

  # Get list of entered trials
  UUIDs_in_trials = trials_archive$UUID %>% unique()
  # Get list of entered runs
  InitializeMain()
  UUIDs_in_runs = run_archive %>% unnest_wider(assignment) %>%
    filter(experiment == group) %>% .$UUID %>% unique()
  trial_counts = run_archive %>% unnest_wider(assignment) %>%
    filter(experiment == group) %>%
    transmute(trials = map_dbl(stats, ~.$trial_count), UUID = UUID)
  trials_total = sum(trial_counts$trials)

  if(nrow(trials_archive) != trials_total) {
    writeLines(glue(" Issue with archive: off by {abs(trials_total-nrow(trials_archive))}"))

    trial_count_comparison = trials_archive %>% group_by(UUID) %>% summarise(rows = length(UUID)) %>%
      left_join(trial_counts, by = "UUID") %>%
      mutate(Issue = rows != trials) # rows is the 'found trials' while trials is the 'expected trials', they should match

    Bad_UUIDs = filter(trial_count_comparison, Issue == TRUE) %>%
      left_join(run_archive %>% select(date, rat_name, rat_ID, file_name, UUID), by = "UUID")

    if(nrow(trial_count_comparison) == 0) {
      writeLines(glue("ISSUE IN {group}_archive.csv.gz
                          NOT backed up"))
    } else {
      # Function ---------------------------------------------------------------_
      clean_wrapper <- function(entry, date) {
        clean_archives(entry, date)

        writeLines("Done.")
        InitializeMain()
        source(paste0(projects_folder, "/admin tools/import_deleted_entries.R"))
      }

      # Bad runs ---------------------------------------------------------------
      print(Bad_UUIDs)
      # clean and Reload

      # Finding missing runs ---------------------------------------------------
      # Find what is missing
      UUIDs_missing = keep(UUIDs_in_runs, ~ ! . %in% UUIDs_in_trials)
      UUIDs_without_runs = keep(UUIDs_in_trials, ~ ! . %in% UUIDs_in_runs)

      if(is_empty(UUIDs_without_runs)) {
        writeLines(glue("          No extra data for {group}"))
      } else {
        writeLines("Removing orphan trial data... ")
        trials_archive = filter(trials_archive, ! UUID %in% UUIDs_without_runs)
        fwrite(trials_archive, file = paste0(projects_folder, group, "_archive.csv.gz"))
        cat("Done.")

        # Find and load orphan trials -----------------------------------------------

        Load_old_file_from_UUID <- function(UUID) {
          # Get date & time from UUID to look for files
          bad_date = stringr::str_sub(UUID, start = 1, end = 8)
          bad_time = stringr::str_sub(UUID, start = 9, end = 14)

          # Determine project from the group we are in
          if (group == "Tsc2-LE") project = "Tsc2 Eker"
          else if (group == "GD") project = "Tsc2 Eker" # some of the earliest data ended up here
          else project = group

          # find location of bad file
          if(bad_date > 20230104) file_location = modern_matlab_file_location
          else file_location = glue("{Box_file_location}/{project}/data")

          # Get possible files
          files = list.files(paste0(file_location, "/", bad_date), pattern = paste0(bad_date, "-", bad_time, "_BOX#\\d{3}\\.mat$"), recursive = TRUE)
          cat(glue("
                        Found {length(files)} possible files..."))

          # Check for old assignments
          old_assignment_check <- function(file) {
            load(paste0(projects_folder, "old_excel_archive.Rdata"))

            rat = stringr::str_extract(file, pattern = "^.+?[:digit:]+?(?=_)")
            # Regex: one or more character (.*+?, ? makes it lazy so first match)
            # followed by one or more digits ([:digit:]+?), followed by but NOT including an _

            old_data = old_excel_archive %>% dplyr::filter(Date == lubridate::ymd(bad_date) & rat_name == rat)

            if(nrow(old_data) == 1) {
              cat(" with data to load")
              return(file)
            } else {
              return(NULL)
            }

          }

          files_checked = lapply(files, old_assignment_check)

          # ID files that can't be auto-delt with because of having multiple or no matches
          if(any(is_null(files_checked))) {
            writeLines(glue("Unable to find entries for: {keep(files_checked, is_null)}"))
            files_checked = discard(files_checked, is_null)
          }

          if(length(files_checked) > 0) {
            cat(glue(" attempting to load {length(files_checked)}"))
            source(paste0(projects_folder, "main.R"))
            lapply(files_checked, Process_File, old_file = TRUE, ignore_name_check = TRUE, exclude_trials = "")
          } else cat(glue("no data found. Can NOT autoload run for
                          {UUID}"))
        }

        writeLines(paste0("\nLoading ", length(UUIDs_without_runs), " files."))
        # try to get files and then run through main
        lapply(UUIDs_without_runs, Load_old_file_from_UUID)
        writeLines("\nDone attempting to load missing runs")

      }


      # Find and load orphan runs -----------------------------------------------

      if(length(UUIDs_missing) > 0 | nrow(Bad_UUIDs) > 0) {
        writeLines(glue("          Missing trials for runs in {group}"))
        # Write out list to load
        bad_runs = run_archive %>% filter(UUID %in% UUIDs_missing | UUID %in% Bad_UUIDs$UUID) %>% arrange(date)

        ## TODO: in-progress
        # 1) need to backup data but not restore assignment through remove entry.R AND save the key_data for autoloading
        # This will need an apply with 1 for rows and will require that the remove entry be in a 'safetied' state
        # 2) reload data from key_data using main with the assignment from the key_data

        # Check prior to cleaning bad entries -------------------------------------
        switch(menu(c("Yes", "No"),
                    title=paste0("Proceed with cleaning bad runs from run_archive?\n
                                 Cleaning removes all rows of the affected runs, then automatically re-imports the original .mat files.\n
                                 Observations, weight, and other hand-entered data will be preserved and reused.
                                 Choose YES if it is possible that some .mat files were imported more than once today."),
                    graphics = FALSE),
               # 1 (Yes): Write file
               lapply(bad_runs %>% .$UUID, clean_wrapper),
               # 2 (No): Abort
               writeLines("Stopped. Entries remain."))
      } else writeLines(glue("No missing trials data for {group}"))
    }
  } else {
    cat(" good...")
    # Backup
    fwrite(trials_archive, file = paste0(projects_folder, group, "_archive.csv.gz.backup"))
    cat(" backed up\n")
  }
}

writeLines("Checking and backing up trials")

archives = list.files(path = projects_folder, pattern = "^.*_archive.csv.gz$") %>% paste0(projects_folder, .) %>% data.frame(file = .) %>%
  mutate(experiment = stringr::str_extract(file, pattern = '(?<=s/).*(?=_a)'))

# NA_archives are problematic these need to found and fixed
# to find them, check for blank experiment
if(any(archives$experiment == "")) {
  warn("NA_archive exists. Clean runs from it manualy.")
  # do not run these through the fixer as the don't need to be backed up.
  archives = filter(archives, experiment != "")
}

archives$experiment %>%
  lapply(Find_Issues_in_Archives)

cat("\nBacking up runs...")
save(run_archive, file = paste0(projects_folder, "run_archive.Rdata.backup"), ascii = FALSE, compress = FALSE)
cat(" Done.")
