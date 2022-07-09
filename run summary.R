
# Omitted Trials ----------------------------------------------------------
# These are typically demos or could trials that are ruled invalid by an observer.

# get trial numbers to omit (these will be in a CSV format)
omit_list = "2, 32, 6"
# cleanse any spaces
omit_list = gsub("[[:space:]]", "", omit_list)
# split string into list
omit_list = strsplit(omit_list,",")

# filter lines out of data
run_data_kept = run_data %>% dplyr::filter(!row_number() %in% omit_list[[1]])

# Sanity check omitted trials ---------------------------------------------
# Make sure the the number of omitted lines matches how many lines were removed from table

# get number of trials omitted
omit_count = length(omit_list[[1]]) %>% as.numeric()
# calculate expected trials
Trials_expected = total_trials - omit_count
# Calculate # of kept trials
Trials_kept = dplyr::count(run_data_kept) %>% as.numeric()
# check
if (Trials_kept != Trials_expected) stop("Omitting Trials - expected kept count does not match")

rm(run_data)

# Basic Stats -------------------------------------------------------------

# Calculate the summary statistics
hits_kept = run_data_kept %>% dplyr::filter(Response == "Hit") %>% dplyr::count() %>% as.numeric()
misses_kept = run_data_kept %>% dplyr::filter(Response == "Miss") %>% dplyr::count() %>% as.numeric()
CRs_kept = run_data_kept %>% dplyr::filter(Response == "CR") %>% dplyr::count() %>% as.numeric()
FAs_kept = run_data_kept %>% dplyr::filter(Response == "FA") %>% dplyr::count() %>% as.numeric()

# cleanup non-omitted summary stats
rm(list = c("CRs_calc", "FAs_calc", "misses_calc", "hits_calc", "Trials_expected"))

# Get percentages for summary sheet
hit_percent = hits_kept / Trials_kept
if (hit_percent <= .75) {
  warning(paste0("Low hit rate: ", round(hit_percent * 100, digits = 1), "%"))
  Warnings = append(Warnings, paste0("Low hit rate: ", round(hit_percent * 100, digits = 1), "%"))
}

FA_percent = FAs_kept / Trials_kept
if (FA_percent >= .3) {
  warning(paste0("High false alarm (FA) rate: ", round(FA_percent * 100, digits = 1), "%"))
  Warnings = append(Warnings, paste0("High false alarm (FA) rate: ", round(FA_percent * 100, digits = 1), "%"))
}

# Get average attempt number for summary sheet
avg_attempts_kept = dplyr::summarise_at(run_data_kept, vars(Attempts_to_complete), mean, na.rm = TRUE)$Attempts_to_complete
