# Find missing entries ---------------------------------------------------

# Load file
Tsc = fread(paste0(projects_folder, "Tsc2-LE_archive.csv.gz"))

# Get list of entered trials
UUIDs_in_trials = Tsc$UUID %>% unique()
# Get list of entered runs
UUIDs_in_runs = run_archive %>% filter(rat_name %in% Tsc_rats) %>% .$UUID %>% unique()
# Find what is missing
UUIDs_missing = keep(UUIDs_in_runs , ~ ! . %in% UUIDs_in_trials)

# Write out list to load
run_archive %>% filter(UUID %in% UUIDs_missing) %>% 
  arrange(desc(date)) %>%
  select(date, rat_name, rat_ID, omit_list, assignment) %>% 
  unnest_wider(assignment) %>% 
  select(-comment) %>%
  unnest(omit_list) %>%
  arrange(date) %>%
  fwrite("missing.csv")


# De-duplicating ----------------------------------------------------------

# Load file
Tsc_original = fread(paste0(projects_folder, "Tsc2-LE_archive.csv.gz"))

# ID duplicates
Dup = duplicated(Tsc_original) | duplicated(Tsc_original, fromLast = TRUE)
Tsc_dup = mutate(Tsc_original, Dup = Dup) %>% rowid_to_column()
filter(Tsc_dup, Dup == TRUE)

# Remove
Tsc = unique(Tsc_original)

# Write file
fwrite(Tsc, paste0(projects_folder, "Tsc2-LE_archive.csv.gz"))
