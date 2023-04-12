group = "Oddball"

# Find missing entries ---------------------------------------------------
# Load file
trials_archive = fread(paste0(projects_folder, group, "_archive.csv.gz"))

# Get list of entered trials
UUIDs_in_trials = trials_archive$UUID %>% unique()
# Get list of entered runs
InitializeMain()
UUIDs_in_runs = run_archive %>% unnest_wider(assignment) %>% filter(experiment == group) %>% .$UUID %>% unique()
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

if(nrow(temp) >= 1) {fwrite(temp, "missing.csv")
  } else { writeLines(glue("\tNo missing data for {group}")) }
  

rm(list = c("trials_archive", "group", "UUIDs_in_trials", "UUIDs_in_runs", "UUIDs_missing", "temp"))


# De-duplicating ----------------------------------------------------------

# Load file
trials_archive = fread(paste0(projects_folder, group, "_archive.csv.gz"))

# ID duplicates
Dup = duplicated(trials_archive) | duplicated(trials_archive, fromLast = TRUE)
trials_archive_dup = mutate(trials_archive, Dup = Dup) %>% rowid_to_column()
filter(Tsc_dup, Dup == TRUE)

# Remove
trials_archive_dedup = unique(trials_archive)

# Write file
fwrite(trials_archive_dedup, paste0(projects_folder, "Tsc2-LE_archive.csv.gz"))

rm(list = c("trials_archive", "group", "Dup", "trials_archive_dup", "trials_archive_dedup"))
