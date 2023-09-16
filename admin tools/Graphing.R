InitializeMain()

rat_to_graph = c("Green21", "Green22", "Green23", "Green24", "Green25", "Green 26")
# "today" or a specific date in YYYYMMDD format
date_to_graph = "today"
# can be reaction or dprime
what_to_graph = "reaction"


date_to_graph = if_else(date_to_graph == "today", str_remove_all(Sys.Date(), "-"), date_to_graph)

Grapher <- function(rat_to_graph) {
  data = run_archive %>%
    filter(rat_name %in% rat_to_graph) %>%
    unnest_wider(assignment) %>%
    unnest_wider(stats) %>%
    unnest(all_of(what_to_graph)) %>%
    rename_at(vars(contains("Freq")), ~ str_extract(., pattern = "Freq")) %>%
    rename_at(vars(contains("dB")), ~ str_extract(., pattern = "dB")) %>%
    rename(value = if_else(what_to_graph == "reaction", "Rxn", what_to_graph))
  
  Today = data %>% filter(date == date_to_graph) 
  
  Freq_filter = unique(Today$Freq)
  Dur_filter = min(Today$`Dur (ms)`)
  today_analysis_type = unique(Today$analysis_type)
  
  data = filter(data, ! task %in% c("Training", "Reset"))

  if(str_detect(today_analysis_type, "Oddball")){
    data = data %>%  
      filter(analysis_type == today_analysis_type)
  } else if((str_detect(today_analysis_type, "BBN"))) {
    data = data %>% 
      filter(analysis_type == today_analysis_type) %>%
      filter(`Dur (ms)` == Dur_filter) 
  } else {
    data = data %>% 
      filter(analysis_type == today_analysis_type) %>%
      filter(Freq == Freq_filter) 
  }
  
  Get_Step_Size <- function(summary) {
    temp = summary$dB_step_size %>% unique
    
    if(length(temp) != 1) {
      step_size = Inf
      # print(temp)
    } else step_size = temp
    
    return(step_size)
  }
  
  data = data %>% rowwise() %>% mutate(step_size = Get_Step_Size(summary) %>% as.factor())
  
  suppressMessages(print(ggplot(data = data, aes(x = dB, y = value)) +
      {if(what_to_graph == "dprime") geom_hline(yintercept = 1.5, color = "blue") } +
      geom_line(data = Today,
                linewidth = 2, color = "darkgreen") +
      geom_point(aes(color = step_size)) +
      geom_smooth(se = FALSE, linewidth = 2, color = "grey") +
        # geom_boxplot(aes(fill = step_size, group = interaction(dB, step_size)), position = position_dodge(4), width = 1, show.legend = FALSE) +
      {if(what_to_graph == "reaction") stat_summary(aes(color = step_size), fun = mean, shape = 18, position = position_dodge(4), size = 1) } +
      labs(title = glue("{what_to_graph} for {rat_to_graph} @ {Freq_filter}kHz"),
           y = paste(what_to_graph),
           color = "Intensity\nSteps") +
      scale_x_continuous(breaks = seq(0, 100, by = 20)) +
      scale_color_manual(values = c("10" = "black", "5" = "coral")) +
      theme_ipsum_es()))
}



lapply(rat_to_graph, Grapher)
