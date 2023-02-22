InitializeMain()

Tsc_rats = c("RP1", "RP2", "RP3", "RP4", "RP5", "RP6", "TP1", "TP2", "TP3", "TP4", "TP5", "TP6", "GP1", "GP2", "GP3", "GP4", "GP5", "GP6")
Fmr_rats = c("BP1", "BP2", "BP3", "BP4", "BP5", "BP6", "LP1", "LP2", "LP3", "LP4", "LP5", "LP6", "Purple1", "Purple2", "Purple3", "Purple4")

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
InitializeMain()
rat_archive %>% filter(is.na(end_date)) %>%
  filter(! Rat_name %in% c(run_archive %>% filter(date == str_remove_all(Sys.Date(), "-")) %>% .$rat_name %>% as.list)) %>%
  .$Rat_name

# Assignments for tomorrow -------------------------------------------------
rat_archive %>% filter(is.na(end_date)) %>% 
  arrange(Box) %>%
  mutate(Changed = ifelse(Assigned_Filename == Old_Assigned_Filename, "", "*")) %>%
  select(Rat_name, Box, Assigned_Filename, Changed, Assigned_Experiment) #%>%
  # write.csv(paste0(user_settings$projects_folder, "files.csv"), row.names = FALSE)



# Single Rat History -------------------------------------------
run_archive %>% filter(rat_name %in% c("TP3")) %>% 
  unnest_wider(assignment) %>%
  unnest_wider(stats) %>%
  mutate(hit_percent = hit_percent * 100, FA_percent = FA_percent * 100) %>%
  select(date, rat_name, weight, trial_count, hit_percent, FA_percent, file_name, experiment, phase, task, detail, warnings_list) %>%
  unnest_wider(warnings_list, names_sep = "_") %>%
  arrange(desc(date))


# Weight ------------------------------------------------------------------
run_archive %>% filter(rat_name %in% c("TP2")) %>% 
  arrange(desc(date)) %>%
  select(date, rat_name, weight)


# Bad Files Yesterday -----------------------------------------------------

run_archive %>% filter(date == "20230201")  %>% 
  filter(str_detect(warnings_list, pattern = "wrong file", negate = FALSE)) %>%
  select(date, rat_name, assignment, warnings_list) %>% 
  unnest_wider(assignment) %>% unnest(warnings_list) %>% View



