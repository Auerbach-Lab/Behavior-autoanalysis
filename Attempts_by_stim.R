# Attempts ----------------------------------------------------------------
# Get average attempts for each stim

summary_attempts = dplyr::group_by(run_data_kept, Stim_ID) %>%
  dplyr::summarise(Attempts_to_complete = mean(`Attempts_to_complete`) %>% round(digits = 1))

# Add it to a summary table
if (exists("run_data_summary")) {
  run_data_summary = dplyr::left_join(x = run_data_summary,
                                      y = summary_attempts,
                                      by = "Stim_ID", all.x = TRUE)
} else {
  run_data_summary = dplyr::left_join(x = dplyr::select(stim_master_list, -Repeat_number),
                                      y = summary_attempts,
                                      by = "Stim_ID", all.x = TRUE)
}

rm(summary_attempts)
