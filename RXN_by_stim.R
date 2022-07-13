# Rxn Time Averaging ------------------------------------------------------

# Get only Hits on Go stimuli
run_data_hits = run_data_kept %>% dplyr::filter(Type == 1 & Response == "Hit")

# Can add only the RXN time for above threshold here

# Get average and convert to ms
summary_rxn = dplyr::group_by(run_data_hits, Stim_ID) %>%
  dplyr::summarise(RXN_time_ms = mean(`Reaction_(s)`) * 1000)

# Add it to a summary table
if (exists("run_data_summary")) {
  run_data_summary = dplyr::left_join(x = run_data_summary,
                                      y = summary_rxn,
                                      by = "Stim_ID", all.x = TRUE)
} else {
  run_data_summary = dplyr::left_join(x = dplyr::select(stim_master_list, -Repeat_number),
                                      y = summary_rxn,
                                      by = "Stim_ID", all.x = TRUE)
}

rm(list = c("summary_rxn", "run_data_hits"))
