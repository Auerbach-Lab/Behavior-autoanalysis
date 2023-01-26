InitializeMain()

# Thresholds --------------------------------------------------------------
run_archive %>% filter(rat_name %in% c("BP1")) %>% 
  select(date, rat_name, stats) %>% 
  unnest_wider(stats) %>% 
  unnest(threshold) %>% 
  arrange(desc(date)) %>%
  select(date, rat_name, TH)

# Runs entered for today --------------------------------------------------
run_archive %>% filter(date == str_remove_all(Sys.Date(), "-")) %>% 
  unnest_wider(stats) %>% unnest_wider(assignment) %>%
  mutate(hit_percent = hit_percent * 100, FA_percent = FA_percent * 100) %>%
  select(date, rat_name, weight, trial_count, hit_percent, FA_percent, file_name, experiment, phase, task, detail) %>%
  View()

# Not loaded today --------------------------------------------------------
rat_archive %>% filter(is.na(end_date)) %>%
  filter(! Rat_name %in% c(run_archive %>% filter(date == str_remove_all(Sys.Date(), "-")) %>% .$rat_name %>% as.list)) %>%
  .$Rat_name

# Assignment --------------------------------------------------------------
run_archive %>% filter(rat_name %in% c("Blue1")) %>% 
  select(date, rat_name, assignment) %>% 
  unnest_wider(assignment) %>% 
  select(-comment)


# Weight ------------------------------------------------------------------
run_archive %>% filter(rat_name %in% c("BP2")) %>% 
  select(date, rat_name, weight)




