InitializeMain()

rat_to_graph = c("BP6", "LP1", "LP2")
date_to_graph = "20230217"
# can be reaction or dprime
what_to_graph = "dprime"


Grapher <- function(rat_to_graph) {
  data = run_archive %>%
    filter(rat_name %in% rat_to_graph) %>%
    unnest_wider(assignment) %>%
    filter(! phase %in% c("Training", "Reset")) %>%
    unnest_wider(stats) %>%
    unnest(what_to_graph) %>%
    rename_at(vars(contains("Freq")), ~ str_extract(., pattern = "Freq")) %>%
    rename_at(vars(contains("dB")), ~ str_extract(., pattern = "dB")) %>%
    rename(value = if_else(what_to_graph == "reaction", "Rxn", what_to_graph))
  
  Today = data %>% filter(date == date_to_graph) 
  
  Freq_filter = Today %>% .$Freq %>% unique()

  data = data %>%  
    filter(Freq == Freq_filter)
  
  
  suppressMessages(print(ggplot(data = data, aes(x = dB, y = value)) +
      geom_hline(yintercept = 1.5, color = "blue") +
      geom_point(color = "black") +
      geom_smooth(se = FALSE, linewidth = 2, color = "grey") +
      geom_line(data = Today,
                linewidth = 2, color = "darkgreen") +
      labs(title = glue("{what_to_graph} for {rat_to_graph} @ {Freq_filter}kHz"),
           y = paste(what_to_graph)) +
      scale_x_continuous(breaks = seq(0, 100, by = 20)) +
      theme_ipsum_es()))
}

lapply(rat_to_graph, Grapher)


# #inidividual data points
# TBD %>%
#   filter(rat_name %in% rat_to_graph & date == date_to_graph) %>%
#   unnest_wider(stats) %>%
#   unnest(dprime) %>%
#   ggplot(aes(x = dB, y = dprime)) +
#   geom_line() +
#   geom_smooth()
