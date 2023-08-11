custom_rats = rat_archive %>%
  filter(is.na(end_date)) %>%
  filter(Assigned_Filename == "" | Assigned_Filename == "ABR") %>%
  # filter(Old_Assigned_Experiment  != "GD") %>%
  # filter(Rat_name %in% c("BP1")) %>%
  # filter(! Rat_ID %in% rats_not_entered_today$Rat_ID) %>%
  filter(str_detect(Rat_name, "Teal")) %>%
  .$Rat_ID

source("supervisor-summarize.R")
