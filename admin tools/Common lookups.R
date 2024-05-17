InitializeMain()


# Food Alerts -------------------------------------------------------------
rat_archive %>%
  # filter to a specific group
  filter(str_detect(Box, pattern = "3.")) %>%
  as_tibble() %>%
  # show only active rats
  filter(is.na(end_date) & start_date < str_remove_all(Sys.Date(), "-")) %>%
  select(Rat_name, Box, Display_Comment) %>%
  filter(Display_Comment != "") %>%
  arrange(Box)


# Single Rat History -------------------------------------------
run_archive %>% filter(rat_name %in% c("Purple3")) %>% 
  unnest_wider(assignment) %>%
  unnest_wider(stats) %>%
  mutate(hit_percent = round(hit_percent * 100, digits = 1), FA_percent = round(FA_percent * 100, digits = 1)) %>%
  select(date, rat_name, weight, trial_count, hit_percent, FA_percent, file_name, experiment, phase, task, detail, warnings_list) %>%
  # unnest_wider(warnings_list, names_sep = "_") %>%
  arrange(desc(date)) %>%
  print(n=20)
  

# Individual Weight ----------------------------------------------------------
run_archive %>% 
  filter(rat_name %in% c("Bronze3")) %>% 
  arrange(desc(date)) %>%
  select(date, rat_ID, rat_name, weight) %>%
  group_by(rat_ID, rat_name) %>%
  do(
    arrange(., desc(date)) %>%
    mutate(max_run_weight = max(weight, na.rm = TRUE),
           max_free_weight = dplyr::filter(rat_archive, Rat_ID == unique(.$rat_ID))$Max_Weight,
           max_weight = max(max_run_weight, max_free_weight, na.rm = TRUE),
           weight_change = glue("{round(((weight - max_weight)/max_weight) * 100, digits = 0)}%")) %>%
      print(n = 14)
  ) 


# Runs entered for today --------------------------------------------------
run_archive %>% filter(date == as.numeric(str_remove_all(Sys.Date(), "-"))) %>% 
  arrange(desc(time)) %>%
  unnest_wider(stats) %>% unnest_wider(assignment) %>%
  mutate(hit_percent = round(hit_percent * 100, digits = 1), FA_percent = round(FA_percent * 100, digits = 1)) %>%
  left_join(rat_archive %>% 
              filter(is.na(end_date) & start_date < str_remove_all(Sys.Date(), "-")) %>% 
              select(Rat_name, Box), 
            by = join_by(rat_name == Rat_name)) %>%
  select(date, Box, rat_name, weight, weightProblem, rxnProblem, scientist,
         trial_count, hit_percent, FA_percent, 
         file_name, experiment, phase, task, detail, analysis_type, comments) %>%
  arrange(Box) %>%
  View


# Not loaded today --------------------------------------------------------
rat_archive %>% filter(is.na(end_date) & start_date <= str_remove_all(Sys.Date(), "-")) %>%
  filter(! Rat_name %in% c(run_archive %>% filter(date == str_remove_all(Sys.Date(), "-")) %>% .$rat_name %>% as.list)) %>%
  .$Rat_name


# Yesterday's Runs and notes ----------------------------------------------
InitializeMain()
yesterday = run_archive %>% filter(date == str_remove_all(Sys.Date() - 1, "-")) %>% 
  arrange(desc(time)) %>%
  unnest_wider(stats) %>% unnest_wider(assignment) %>%
  mutate(hit_percent = round(hit_percent * 100, digits = 1), FA_percent = round(FA_percent * 100, digits = 1)) %>%
  left_join(rat_archive %>% 
              filter(is.na(end_date) & start_date < str_remove_all(Sys.Date(), "-")) %>% 
              select(Rat_name, Box), 
            by = join_by(rat_name == Rat_name)) %>%
  select(date, Box, rat_name, weight, weightProblem, rxnProblem, 
         trial_count, hit_percent, FA_percent, 
         file_name, experiment, phase, task, detail, scientist) %>%
  arrange(Box)
View(yesterday)


# Bad Files Yesterday -----------------------------------------------------
InitializeMain()
run_archive %>% filter(date == "20230201")  %>% 
  filter(str_detect(warnings_list, pattern = "wrong file", negate = FALSE)) %>%
  select(date, rat_name, assignment, warnings_list) %>% 
  unnest_wider(assignment) %>% unnest(warnings_list) %>% View


# Thresholds --------------------------------------------------------------
InitializeMain()
run_archive %>% filter(rat_name %in% c("Red1")) %>% 
  select(date, rat_name, stats) %>% 
  unnest_wider(stats) %>% 
  unnest(threshold) %>% 
  arrange(desc(date)) %>%
  select(date, rat_name, TH)


# Oddball Counts ----------------------------------------------------------
InitializeMain()
run_archive %>% dplyr::filter(rat_name == "LP3") %>% dplyr::arrange(date) %>%
  tidyr::unnest_wider(assignment) %>%
  dplyr::filter(experiment == "Oddball" & invalid != TRUE) %>%
  filter(! task %in% c("Training")) %>%
  # Get go frequency by extracting 1st (^) number ([:digit:]+) from file_name
  mutate(frequency = str_extract(file_name, pattern = "^[:digit:]+") %>% as.numeric()) %>%
  group_by(task, detail, frequency) %>%
  summarise(task = unique(task), frequency = unique(frequency), detail = unique(detail),
            date = tail(date, 1), n = n(),
            .groups = "drop") %>%
  arrange(task, frequency)


# Catch Trial dates -------------------------------------------------------
run_archive %>% 
  select(rat_name, rat_ID, date, file_name, assignment, invalid) %>%
  unnest_wider(assignment) %>%
  filter(experiment %in% c("Oddball") & task %in% c("Catch trials", "Probe trials") & invalid != TRUE & rat_ID > 101) %>% 
  arrange(desc(date)) %>%
  select(-comment, -detail, -assigned_file_name, -invalid) %>%
  left_join(select(rat_archive, c(Rat_ID, Genotype)), by = c("rat_ID" = "Rat_ID")) %>%
  fwrite(paste0("C:/Users/Noelle/Box/Behavior Lab/Shared/Ben/Catch_trials_data_exported_", Sys.Date(),".csv"), row.names = FALSE)


# Reaction times for a specific day ---------------------------------------
run_archive %>% 
  filter(rat_name %in% c("Green21") & date %in% c(20231129)) %>% 
  unnest_wider(assignment) %>%
  unnest_wider(stats) %>%
  mutate(hit_percent = round(hit_percent * 100, digits = 1), FA_percent = round(FA_percent * 100, digits = 1)) %>% 
  unnest(reaction) %>% 
  View()


# Rats Weight change today -----------------------------------------
InitializeMain()
run_archive %>%
  select(date, rat_ID, rat_name, weight) %>%
  filter(rat_ID %in% c(rat_archive %>% 
                         filter(is.na(end_date) & start_date < str_remove_all(Sys.Date(), "-")) %>%
                         .$Rat_ID)) %>%
  group_by(rat_ID, rat_name) %>%
  do(
    arrange(., desc(date)) %>%
      mutate(max_run_weight = max(weight, na.rm = TRUE),
             max_free_weight = dplyr::filter(rat_archive, Rat_ID == unique(.$rat_ID))$Max_Weight,
             max_weight = max(max_run_weight, max_free_weight, na.rm = TRUE),
             weight_change = glue("{round(((weight - max_weight)/max_weight) * 100, digits = 0)}%")) %>%
      head(n = 1)
  ) %>% left_join(select(rat_archive, Rat_ID, Box), by = join_by(rat_ID == Rat_ID)) %>%
  # select a specific group
  filter(str_detect(Box, pattern = "[5-6].")) %>%
  arrange(Box) %>% View
