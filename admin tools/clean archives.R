clean_archives <- function(entry, date) {
  df = run_archive %>% filter(UUID == entry)
  writeLines(paste0("\t\tCleaning ", df$rat_name, "'s entry on ", df$date, " ..."))

  # Backup lose-able data
  # Get data to save
  key_data = df %>%
    select(all_of(c("date", "rat_ID", "rat_name", "weight", "omit_list", "assignment", "comments", "scientist", "weightProblem", "rxnProblem", "UUID"))) %>%
    unnest_wider(assignment) %>%
    mutate(date_removed = Sys.Date() %>% as.character(),
           omit_list = omit_list)
  # append to running CSV
  tryCatch(
    fwrite(key_data, file = paste0(projects_folder, "deleted_entries.csv"), append = file.exists(paste0(projects_folder, "deleted_entries.csv"))),
    error = function(e) { # this function name is specific to tryCatch and cannot be changed
      warning("Fwrite error - Could not write to deleted_entries.csv?")
      message("Original error message:")
      warning(e)
      message("Output from fwrite should have been:")
      fwrite(key_data, file = "")
      stop("ABORT - fwrite error.")
      return(NA)
    },
    warning=function(e) {
      warning("Fwrite warning.")
      message("Original warning message:")
      warning(e)
      message("Output from fwrite should have been:")
      fwrite(key_data, file = "")
      stop("ABORT - fwrite warning.")
      return(NA)
    }
  )

  # Wipe from trials archive:
  experiment = df$assignment %>% .[[1]] %>% pluck("experiment")
  variable_name = paste0(experiment, "_archive")
  filename = paste0(projects_folder, variable_name, ".csv.gz")
  # load archive
  trial_archive = fread(filename)
  # clean archive
  temp = filter(trial_archive, UUID != df$UUID)
  # save cleaned archive
  fwrite(temp, file = filename)
  writeLines(paste0("\tTrials removed from ", variable_name))

  # Wipe UUID from run archive
  run_archive = filter(run_archive, UUID != df$UUID)
  save(run_archive, file = paste0(projects_folder, "run_archive.Rdata"), ascii = FALSE, compress = FALSE)
  writeLines("\tRun archive cleaned")
}
