

# Run summary -------------------------------------------------------------


# Attempts ----------------------------------------------------------------
# Get average attempts for each stim
summary_attempts = dplyr::group_by(run_data_kept, Stim_ID) %>%
  dplyr::summarise(Attempts_to_complete = mean(`Attempts_to_complete`) %>% round(digits = 1))

run_data_summary = dplyr::left_join(x = dplyr::select(stim_master_list, -Repeat_number),
                                    y = summary_attempts,
                                    by = "Stim_ID", all.x = TRUE)

rm(summary_attempts)


# Hit % -------------------------------------------------------------------
# Get hit % for each stim

summary_hits = dplyr::group_by(run_data_kept, Stim_ID) %>%
  dplyr::summarise(Hits = sum(Response == "Hit"),
                   Misses = sum(Response == "Miss"),
                   CRs = sum(Response == "CR"),
                   FAs = sum(Response == "FA"),
                   hit_percent = sum(Response == "Hit") / sum(Response == "Hit" | Response == "Miss") %>% round(digits = 3))

run_data_summary = dplyr::left_join(x = run_data_summary,
                                    y = summary_hits,
                                    by = "Stim_ID", all.x = TRUE)

rm(summary_hits)

# Rxn Time Averaging ------------------------------------------------------

# Get only Hits on Go stimuli
run_data_hits = run_data_kept %>% dplyr::filter(Type == 1 & Response == "Hit")

summary_rxn = dplyr::group_by(run_data_hits, Stim_ID) %>%
  dplyr::summarise(RXN_time_ms = mean(`Reaction_(s)`) * 1000)

run_data_summary = dplyr::left_join(x = run_data_summary,
                                    y = summary_rxn,
                                    by = "Stim_ID", all.x = TRUE)

rm(list = c("summary_rxn", "run_data_hits"))



# Combine for summary table -----------------------------------------------

