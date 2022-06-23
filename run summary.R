
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

## Sanity Check ##

# get number of trials omitted
omit_count = length(omit_list[[1]]) %>% as.numeric()
# calculate expected trials
Trial_expected = total_trials - omit_count
# Calculate # of kept trials
Trials_kept = dplyr::count(run_data_kept) %>% as.numeric()
# check
if (Trials_kept != Trial_expected) stop("Omitting Trials - expected kept count does not match")

# Basic Stats -------------------------------------------------------------


hit_percent = hits_calc / total_trials
FA_percent = hits_calc / total_trials
