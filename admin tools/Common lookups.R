
# Thresholds --------------------------------------------------------------
run_archive %>% filter(rat_name %in% c("GP6")) %>% 
  select(date, rat_name, stats) %>% 
  unnest_wider(stats) %>% 
  unnest(threshold) %>% 
  select(date, rat_name, TH)


# Assignment --------------------------------------------------------------
run_archive %>% filter(rat_name %in% c("Purple1")) %>% 
  select(date, rat_name, assignment) %>% 
  unnest_wider(assignment) %>% 
  select(-comment)


# Weight ------------------------------------------------------------------
run_archive %>% filter(rat_name %in% c("Blue2")) %>% 
  select(date, rat_name, weight)

