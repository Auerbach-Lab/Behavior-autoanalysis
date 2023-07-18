
# User selections ---------------------------------------------------------
# these are variables used in the script

# Experiment(s) to export
experiment_to_keep = c("Oddball")

# Do not include records before the following date
# Date format should be: YYYYMMDD
# for all records, comment out this line or set it to prior to 20000101
start_date_of_records = 20230210

# location to save file to:
save_location = "C:/Users/Noelle/Desktop/"


# Script ------------------------------------------------------------------

run_archive %>%
  unnest_wider(assignment) %>%
  filter(experiment %in% experiment_to_keep & rat_ID > 101) %>%
  mutate(warnings_list = unlist(str_flatten_comma(warnings_list))) %>%
  .[-97,] %>%
  unnest_wider(summary) %>% 
  unnest_wider(stats) %>% 
  unnest(c(dprime, reaction, FA_detailed), names_sep = "_") %>% 
  fwrite(paste0(save_location, experiment_to_keep, "_data_exported_", Sys.Date(),".csv"), row.names = FALSE)
