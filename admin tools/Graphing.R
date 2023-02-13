InitializeMain()

rat_to_graph = c("BP6")
date_to_graph = "20230210"


run_archive %>%
  filter(rat_name %in% rat_to_graph & date == date_to_graph) %>%
  unnest_wider(stats) %>%
  unnest(reaction) %>%
  mutate(Rxn = Rxn * 1000) %>%
  ggplot(aes(x = `Inten (dB)`, y = Rxn)) +
    geom_line()


run_archive %>%
  filter(rat_name %in% rat_to_graph & date == date_to_graph) %>%
  unnest_wider(stats) %>%
  unnest(dprime) %>%
  ggplot(aes(x = dB, y = dprime)) +
    geom_line()


# #inidividual data points
# TBD %>%
#   filter(rat_name %in% rat_to_graph & date == date_to_graph) %>%
#   unnest_wider(stats) %>%
#   unnest(dprime) %>%
#   ggplot(aes(x = dB, y = dprime)) +
#   geom_line() +
#   geom_smooth()
