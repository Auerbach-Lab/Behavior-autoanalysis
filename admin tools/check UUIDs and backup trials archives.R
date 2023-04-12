Check_Trial_Archives <- function () {
  Archive_Reader <- function(df) {
    archive_file = df["file"]
    experiment = df["experiment"]
    cat(glue("\t{experiment}..."))
    archive = fread(archive_file)
    r = 0

    # Check file
    if (nrow(archive) >= 2 & length(names(archive)) >= 18) {
      r = unique(archive$UUID)
      cat(glue(" {length(r)} entries..."))
    } else {
      stop(glue("ABORT: Problem with {archive_file}."))
    }

    # Backup
    fwrite(archive, file = paste0(archive_file, ".backup"))
    cat(" backed up\n")
    return(r)
  }

  # Get archives
  archives = list.files(path = projects_folder, pattern = "^.*_archive.csv.gz$") %>% paste0(projects_folder, .) %>%
    data.frame(file = .) %>% mutate(experiment = str_extract(file, pattern = '(?<=s/).*(?=_a)'))     # Get just the experiment name
  writeLines("Checking and backing up trials archives")
  if(nrow(archives) == 0) writeLines("Nothing to backup")
  else {
    trials_uuids = do.call(c, apply(archives, 1, Archive_Reader))
    trials_uuid_counts = length(trials_uuids)
    runs_uuid_counts = length(unique(run_archive$UUID))
    diff = setdiff(trials_uuids, unique(run_archive$UUID))

    if (length(diff) == 0) {
      writeLines(glue("Runs and trials agree: {trials_uuid_counts} UUIDs."))
    } else {
      difference = setdiff(trials_uuids, unique(run_archive$UUID))
      writeLines("The following UUIDs were in one but not the other:")
      writeLines(glue("\t{difference}"))
      warn = glue("ACTION REQUIRED: {trials_uuid_counts} UUIDs found across trials archives, but {runs_uuid_counts} found in run_archive.")
      warning(paste0(warn, "\n"))
    }
  }
}

Check_Trial_Archives()
