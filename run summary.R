
# Omitted Trials ----------------------------------------------------------
# These are typically demos or could trials that are ruled invalid by an observer.

# get trial numbers to omit (these will be in a CSV format)
omit_list = "2, 32, 6"
# cleanse any spaces
omit_list = gsub("[[:space:]]", "", omit_list)
# split string into list
omit_list = strsplit(omit_list,",")

# filter lines out of data
run_data_keep = run_data %>% dplyr::filter(!row_number() %in% omit_list)


# get number of trials omitted
omit_count = length(omit_list[[1]]) %>% as.numeric()

# Basic Stats -------------------------------------------------------------


hit_percent = hits_calc / total_trials
FA_percent = hits_calc / total_trials
