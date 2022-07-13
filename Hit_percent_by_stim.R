# Hit % -------------------------------------------------------------------
# Get hit % for each stim

summary_hits = dplyr::group_by(run_data_kept, Stim_ID) %>%
  dplyr::summarise(Hits = sum(Response == "Hit"),
                   Misses = sum(Response == "Miss"),
                   CRs = sum(Response == "CR"),
                   FAs = sum(Response == "FA"),
                   hit_percent = sum(Response == "Hit") / sum(Response == "Hit" | Response == "Miss") %>% round(digits = 3))

# Add it to a summary table
if (exists(run_data_summary)) {
  run_data_summary = dplyr::left_join(x = run_data_summary,
                                      y = summary_hits,
                                      by = "Stim_ID", all.x = TRUE)
} else {
  run_data_summary = dplyr::left_join(x = dplyr::select(stim_master_list, -Repeat_number),
                                      y = summary_hits,
                                      by = "Stim_ID", all.x = TRUE)
}

rm(summary_hits)
