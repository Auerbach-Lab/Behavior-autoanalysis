InitializeMain()

# Clear extant plots
if(!is_null(dev.list()["RStudioGD"])) dev.off(dev.list()["RStudioGD"]) 

Weight_Grapher <- function(rat_to_graph) {
  rat_runs = run_archive %>% dplyr::filter(rat_ID == rat_to_graph)
  max_weight = max(rat_runs$weight, na.rm = TRUE)
  rat_runs = rat_runs %>% filter(date > str_remove_all(Sys.Date()-30, "-")) %>%
    mutate(date_asDate = lubridate::ymd(date),
           trial_count = map_dbl(stats, ~.$trial_count),
           weight_change = (weight - max_weight)/max_weight,
           weight_annotation = if_else(abs(weight_change) > 0.15, paste0(round((weight_change)*100, digits = 0), "%"), NULL))
  
  min_trials = pluck(user_settings, "minimum_trials", 
                     arrange(rat_runs, desc(date)) %>% head(n = 1) %>% .$analysis_type)
  
  suppressMessages(print(
      rat_runs %>% 
      ggplot(aes(x = date_asDate, y = weight)) +
      geom_hline(yintercept = min_trials, linetype = "longdash") +
      geom_smooth(color = "grey", linewidth = 2,
                  se = FALSE, na.rm = TRUE, method = "lm", formula= y~x) +
      geom_point(shape=21, color="black", fill="#69b3a2", size=5) +
      geom_text(aes(x = min(date_asDate) + 1, y = max(weight) + 10), label = "Weight", color="#69b3a2")+
      geom_smooth(aes(x = date_asDate, y = trial_count),
                  color = "black", linewidth = 2,
                  se = FALSE, na.rm = TRUE, method = "lm", formula= y~x) +
      geom_text(data = filter(rat_runs, !is.na(weight_annotation)),
                aes(label = weight_annotation), color="darkred", nudge_y = 20, size = 3)+
      geom_text(aes(x = min(date_asDate) + 1, y = mean(trial_count) + 3), label = "Trial trend")+
      ggtitle(glue("{unique(rat_runs$rat_name)} 2 week Weight vs. Trail count")) +
      scale_y_continuous(expand = c(0.1, 0)) +
      theme_ipsum_es() +
      labs(x = "Date", y = "Weight & Trial Count")
    ))
}

cat("Graphing...")
rat_archive %>%
  filter(is.na(end_date)) %>%
  #select rats that have NOT been run today
  # filter(Assigned_Filename != "") %>%
  # filter(Assigned_Experiment  == "Tsc2-LE") %>%
  # filter(!Rat_name %in% c("Teal3", "Teal4")) %>%
  filter(str_detect(Box, "2.")) %>%
  arrange(desc(Box)) %>%
  .$Rat_ID %>%
  lapply(Weight_Grapher)
cat(" Done")
