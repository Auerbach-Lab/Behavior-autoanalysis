InitializeMain()

# Clear extant plots
if(!is_null(dev.list()["RStudioGD"])) dev.off(dev.list()["RStudioGD"])

Weight_Grapher <- function(rat_to_graph) {
  rat_runs = run_archive %>% dplyr::filter(rat_ID == rat_to_graph)
  weightProblem = rat_runs %>%
    arrange(desc(date)) %>%
    head(n = 1) %>%
    .$weightProblem
  
  max_weight_runs = max(rat_runs$weight, na.rm = TRUE)
  max_weight_freefeed = dplyr::filter(rat_archive, Rat_ID == rat_to_graph)$Max_Weight
  max_weight = max(max_weight_runs, max_weight_freefeed)
  rat_runs = rat_runs %>% filter(date > str_remove_all(Sys.Date()-21, "-")) %>%
    mutate(date_asDate = lubridate::ymd(date),
           trial_count = map_dbl(stats, ~.$trial_count)) %>%
    summarise(date_asDate = unique(date_asDate), 
              analysis_type = unique(analysis_type), rat_name = unique(rat_name),
              trial_count = sum(trial_count), weight = unique(weight),
              .by = c(date)) %>%
    mutate(weight_change = (weight - max_weight)/max_weight,
           weight_annotation = if_else(abs(weight_change) > 0.15, paste0(round((weight_change)*100, digits = 0), "%"), ""))
  
  min_trials = pluck(user_settings, "minimum_trials", 
                     arrange(rat_runs, desc(date)) %>% head(n = 1) %>% .$analysis_type)
  annotation_x = max(rat_runs$weight)
  
  trials_model = lm(trial_count ~ date_asDate, data= rat_runs)
  trails_label_x = predict(trials_model)[[1]]
  
  
  suppressMessages(print(
    rat_runs %>% 
      ggplot(aes(x = date_asDate, y = weight)) +
      # min trials line
      geom_hline(yintercept = min_trials, linetype = "longdash") +
      geom_text(aes(x = min(date_asDate), y = min_trials - 5), label = "Minimum trials")+
      # max weight point
      geom_point(aes(x = min(date_asDate), y = max_weight), fill = "black", size = 5, position = position_nudge(x = -0.5)) +
      # weight trend line
      geom_smooth(color = "grey", linewidth = 2,
                  se = FALSE, na.rm = TRUE, method = "lm", formula= y~x) +
      geom_point(shape=21, color="black", fill="#69b3a2", size=5) +
      geom_text(aes(x = min(date_asDate), y = annotation_x), nudge_x = -0.5, label = "Weight", color="#69b3a2")+
      # trial trend line (Note we don't show all the trial point because they have great variation)
      geom_smooth(aes(x = date_asDate, y = trial_count),
                  color = "black", linewidth = 2,
                  se = FALSE, na.rm = TRUE, method = "lm", formula= y~x) +
      geom_text(aes(x = min(date_asDate), y = trails_label_x + 6), label = "Trial trend")+
      # annotations to weight points only if weight is 15%+ off max weight (Note we want this on top of other layers)
      geom_text(data = filter(rat_runs, !is.na(weight_annotation)),
                aes(y = annotation_x + 6, label = weight_annotation), color="darkred", size = 3) +
      # 3 most recent trial counts used to see if pulling up or down the trend
      geom_point(data = tail(arrange(rat_runs, date_asDate), n = 5), aes(y = trial_count), color = "blue", show.legend = FALSE) +
      # lower the bottom of the y scale as it can be anchored to minimum trials and the label is bellow the line
      scale_y_continuous(expand = c(0.1, 0)) +
      scale_x_date(date_breaks = "4 day", date_minor_breaks = "2 day",
                   date_labels = "%m-%d") +
      labs(title = glue("{unique(rat_runs$rat_name)} three week Weight vs. Trail count"),
           x = "Date", y = "Weight & Trial Count",
           caption = glue("Todays weight comment: {weightProblem}\nMax weight = {max_weight}, Free Feed weight = {max_weight_freefeed}")) +
      theme_ipsum_es()
  ))
}

cat("Graphing...")
rat_archive %>%
  # select active rats
  filter(is.na(end_date) & start_date < str_remove_all(Sys.Date(), "-")) %>%
  # select rats that have NOT been run today
  # filter(Assigned_Filename != "") %>%
  # filter(Assigned_Experiment  == "Tsc2-LE") %>%
  # filter(Rat_name %in% c("Purple1", "Purple2")) %>%
  filter(str_detect(Box, "[1-2].")) %>%
  filter(str_detect(Box, "[1].")) %>% # select by group
  arrange(desc(Box)) %>%
  .$Rat_ID %>%
  lapply(Weight_Grapher)
cat(" Done")
