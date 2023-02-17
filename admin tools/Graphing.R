InitializeMain()

rat_to_graph = c("Purple2")
date_to_graph = "20230217"


run_archive %>%
  filter(rat_name %in% rat_to_graph & date == date_to_graph) %>%
  unnest_wider(stats) %>%
  unnest(reaction) %>%
  mutate(Rxn = Rxn * 1000) %>%
  ggplot(aes(x = `Inten (dB)`, y = Rxn)) +
    geom_line(linewidth = 2, color = "darkgreen") +
    theme_ipsum_es()


run_archive %>%
  filter(rat_name %in% rat_to_graph & date == date_to_graph) %>%
  unnest_wider(stats) %>%
  unnest(dprime) %>%
  ggplot(aes(x = dB, y = dprime)) +
    geom_line(linewidth = 2, color = "darkgreen") +
    theme_ipsum_es()



# #inidividual data points
# TBD %>%
#   filter(rat_name %in% rat_to_graph & date == date_to_graph) %>%
#   unnest_wider(stats) %>%
#   unnest(dprime) %>%
#   ggplot(aes(x = dB, y = dprime)) +
#   geom_line() +
#   geom_smooth()
