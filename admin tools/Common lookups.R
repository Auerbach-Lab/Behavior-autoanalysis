InitializeMain()

# Food Alerts -------------------------------------------------------------
# Show rats with standing notes on feeding. This does not show comments entered on a specific day's feeding.

rat_archive %>%
  # filter to a specific group; comment out with a # to show all groups
    filter(str_detect(Box, pattern = "3.")) %>%
  as_tibble() %>% # converting to tibble for familiar formatting
  filter(is.na(end_date) & start_date < str_remove_all(Sys.Date(), "-")) %>% # show only active rats
  select(Rat_name, Box, Display_Comment) %>%  # select data to be shown
  filter(Display_Comment != "") %>% # hide rats with no comments (i.e. blanks)
  arrange(Box) # sort by box order


# Single Rat History -------------------------------------------
# Show summary data for each run as automatically calculated on run entry
# similar to supervisor summarize

run_archive %>% 
  # select specific rat by name (capital and space sensitive)
    filter(rat_name %in% c("Green6")) %>% 
  unnest_wider(assignment) %>% unnest_wider(stats) %>% # unpack data to show
  mutate(hit_percent = round(hit_percent * 100, digits = 1), FA_percent = round(FA_percent * 100, digits = 1)) %>% # convert and round percentages for easier reading 
  select(date, rat_name, weight, trial_count, hit_percent, FA_percent, 
         file_name, experiment, phase, task, detail, warnings_list, weightProblem, rxnProblem) %>% # select which columns are displayed
  # If you want each warning separated rather than as a list un-comment the following line
    # unnest_wider(warnings_list, names_sep = "_") %>%
  arrange(desc(date)) %>% # sort by descending date order
  print(n=20)             # show 20 entries in the command console
  

# Individual Weight ----------------------------------------------------------
# Show a rat's weight history, typically for vet and health checks

run_archive %>% 
  # select specific rat by name (capital and space sensitive)
    filter(rat_name %in% c("Orange4")) %>% 
  arrange(desc(date)) %>%                     # sort by descending date order
  select(date, rat_ID, rat_name, weight) %>%  # select data to show
  group_by(rat_ID, rat_name) %>%   # calculate change in weight for each rat
  do(
    arrange(., desc(date)) %>% # arrange in descending date order
    mutate(max_run_weight = max(weight, na.rm = TRUE), # get max weight from run list (must be dune prior to pruning)
           max_free_weight = dplyr::filter(rat_archive, Rat_ID == unique(.$rat_ID))$Max_Weight, # pull override/free-feed weight from rat_archive
           max_weight = max(max_run_weight, max_free_weight, na.rm = TRUE), # figure out true max weight
           weight_change = glue("{round(((weight - max_weight)/max_weight) * 100, digits = 0)}%")) %>% # calculate weight change for each day
      print(n = 14) # limit to the last ~2 weeks of entries (days with multiple entries will reduce this)
  ) 


# Runs entered for today --------------------------------------------------
# Show all the runs that have been entered today. Will open as a new table in a separate tab.
# As a named table to that the tab/window has an informative name

runs_entered_today = 
  run_archive %>% filter(date == as.numeric(str_remove_all(Sys.Date(), "-"))) %>%   # show only runs from today's date
  unnest_wider(stats) %>% unnest_wider(assignment) %>%    # unpack data to show
  mutate(hit_percent = round(hit_percent * 100, digits = 1), FA_percent = round(FA_percent * 100, digits = 1)) %>% # convert and round percentages for easier reading 
  # Get active rat info including group & box
  left_join(rat_archive %>% 
              filter(is.na(end_date) & start_date < str_remove_all(Sys.Date(), "-")) %>%  # show only active rats
              select(Rat_name, Box), 
            by = join_by(rat_name == Rat_name)) %>%
  # select data to show
  select(date, Box, rat_name, weight, weightProblem, rxnProblem, scientist,
         trial_count, hit_percent, FA_percent, 
         file_name, experiment, phase, task, detail, analysis_type, comments) %>%
  arrange(Box)# sort by box order
View(runs_entered_today)


# Not loaded today --------------------------------------------------------
# find rats who do NOT have data entered today. Should not require any changes

rat_archive %>% filter(is.na(end_date) & start_date <= str_remove_all(Sys.Date(), "-")) %>% # get list of active rats
  filter(! Rat_name %in% c(run_archive %>% filter(date == str_remove_all(Sys.Date(), "-")) %>% .$rat_name %>% as.list)) %>% # filter out rats with runs entered today
  .$Rat_name


# Yesterday's Runs and notes ----------------------------------------------
# Show all the runs that were entered yesterday. Will open as a new table in a separate tab.
# As a named table to that the tab/window has an informative name

runs_entered_yesterday = run_archive %>% filter(date == str_remove_all(Sys.Date() - 1, "-")) %>% 
  unnest_wider(stats) %>% unnest_wider(assignment) %>%   # unpack data to show
  mutate(hit_percent = round(hit_percent * 100, digits = 1), FA_percent = round(FA_percent * 100, digits = 1)) %>% # convert and round percentages for easier reading 
  # Get active rat info including group & box
  left_join(rat_archive %>% 
              filter(is.na(end_date) & start_date < str_remove_all(Sys.Date(), "-")) %>% 
              select(Rat_name, Box), 
            by = join_by(rat_name == Rat_name)) %>%
  # select data to show
  select(date, Box, rat_name, weight, weightProblem, rxnProblem, 
         trial_count, hit_percent, FA_percent, 
         file_name, experiment, phase, task, detail, scientist) %>%
  arrange(Box) # sort by box order
View(runs_entered_yesterday) # open table to view


# Thresholds --------------------------------------------------------------
# Get a list of daily thresholds for select rats
# TODO: Specify which threshold goes with what condition for files with multiple thresholds

run_archive %>% 
  # select specific rat by name (capital and space sensitive)
    filter(rat_name %in% c("Red1")) %>% 
  mutate(threshold = map_vec(stats, ~ .['threshold']), # retrieve threshold table
         TH = map_vec(threshold, ~ .['TH'] %>% unique() %>% # for situations like multiple durations, there may be more than one threshold
                        round(digits = 2) %>% str_flatten_comma())) %>%    # reduce to the minimum number of thresholds but for some files, like BBN mixed duration, there is more than one threshold
  arrange(desc(date)) %>% # sort by descending date order
  select(date, rat_name, rat_ID, TH) # select data to show


# Oddball Counts ----------------------------------------------------------
# Breakdown of trial counts for a given rat by go and no-go frequencies

run_archive %>% 
  # select specific rat by name (capital and space sensitive)
    dplyr::filter(rat_name == "LP3") %>% 
  dplyr::filter(str_detect(analysis_type, pattern = "Oddball")) %>% # select only Oddball trials
  tidyr::unnest_wider(assignment) %>% # get assignment details
  filter(! task %in% c("Training") & invalid != "TRUE") %>% # remove training and invalid trials
  mutate(go = str_extract(file_name, pattern = "^[:digit:]+") %>% as.numeric(), # Get go frequency by extracting 1st (^) number ([:digit:]+) from file_name
         no_go = str_extract(file_name, pattern = "(?<=_)(BBN|[:digit:]+kHz)(?=_)")) %>% # Get No go frequency by extracting either BBN or the 2nd kHz ([:digit:]+) from file_name
  reframe(n = n(), # get count
          .by = c(task, detail, go, no_go)) # for each task and detail
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
