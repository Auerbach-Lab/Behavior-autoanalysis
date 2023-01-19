
# Thresholds --------------------------------------------------------------
run_archive %>% filter(rat_name == "BP5") %>% 
  select(date, rat_ID, stats) %>% 
  unnest_wider(stats) %>% 
  unnest(threshold) %>% 
  select(date, rat_ID, TH)
