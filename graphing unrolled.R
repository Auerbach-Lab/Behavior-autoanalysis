Generate_Graph <- function(rat_name, ratID) {
  # Thoughts ----------------------------------------------------------------
  # See graphing table for breakdown
  # ISSUE: BBN & Tone mixed would have 3 lines that have similar ranges.
  #        Therefore, I will graph 50ms for Mixed BBN and 8kHz for Mixed Tones.
  
  # Functions ---------------------------------------------------------------
  
  Range_Grapher <- function(plot) {
    # Range Graph for points
    plot +
      stat_summary(aes(color = "Average"),
                   fun = mean,
                   fun.min = function(x) mean(x) - sd(x),
                   fun.max = function(x) mean(x) + sd(x),
                   geom = "errorbar", linewidth = 10, width = 0) +
      geom_point(data = today_graph_data, shape = 21, color = "black", fill = "mediumslateblue", size = 5) +
      # Produces warning about group aesthetic b/c only 1 but needed for nice legend
      geom_line(data = today_graph_data, aes(color = "Today", fill = "Today"), linewidth = 1.5) +
      { if(current_task == "Training") {annotate(x = pluck(today_graph_data, paste0(what_to_graph, "$", x_column)),
                                                 y = pluck(today_graph_data, paste0(what_to_graph, "$", y_column)), 
                                                 label = "Training", geom = "text", hjust = -0.5)} } +
      ggtitle(glue("{rat_name} {what_to_graph} Range Check")) +
      scale_color_manual(values = c("Today" = "mediumslateblue", "Average" = "thistle"), name = "") +
      coord_cartesian(clip = "off") +
      theme_ipsum_es() +
      theme(legend.position = "bottom")
  }
  
  Line_Grapher <- function(plot) {
    # Line Graph for Curves
    plot +
      stat_summary(aes(color = "Average"),
                   fun = mean,
                   fun.min = function(x) mean(x) - sd(x),
                   fun.max = function(x) mean(x) + sd(x),
                   geom = "ribbon", linewidth = 2, fill = "thistle", show_guide=FALSE) +
      { if (exists("TH")) geom_vline(xintercept = TH, linetype = "dashed") } +
      { if (exists("TH")) annotate(x = TH, y = Inf, label = "Threshold", geom = "text", vjust = -0.5) } +
      geom_line(data = today_graph_data, aes(color = "Today"), linewidth = 1.5) +
      geom_point(data = today_graph_data, shape=21, color="black", fill = "mediumslateblue", size = 5) +
      ggtitle(glue("{rat_name} {what_to_graph} Curve Check")) +
      scale_color_manual(values = c("Today" = "mediumslateblue", "Average" = "thistle"), name = "") +
      coord_cartesian(clip = "off") +
      theme_ipsum_es() +
      theme(legend.position = "bottom") 
  }
  
  Blank_Grapher <- function(plot) {
    plot + 
      xlim(0, 100) + 
      ylim(0, 100) + 
      ggtitle(glue("{rat_name} {what_to_graph} does not exist")) +
      theme_ipsum_es() +
      theme(legend.position = "bottom")
  }
  
  # Graph Data generation ---------------------------------------------------
  rat_runs = run_archive %>% dplyr::filter(rat_ID == ratID)
  rat_runs = rat_runs %>% mutate(date_asDate = lubridate::ymd(date)) %>% arrange(desc(date))
  
  # only want to graph relevant data so need today's data
  today_data = rat_runs %>% 
    filter(date == set_date) %>%
    head(graph_data, n = 1) # this should probably be a filter date
  current_analysis_type = today_data$analysis_type
  current_phase = pluck(today_data, "assignment", 1, "phase")
  current_task = pluck(today_data, "assignment", 1, "task")
  current_detail = pluck(today_data, "assignment", 1, "detail")
  current_frequencies = pluck(today_data, "summary", 1, "Freq (kHz)") %>% unique %>% as.list()
  today_graph_data = today_data %>% unnest_wider(stats) %>% unnest(dprime) %>% unnest(reaction)
  
  # minimize data to mess with
  # need table with date, hit%, Frequency, Duration, dB, rxn, dprime 
  graph_data =
    rat_runs %>%
    unnest_wider(assignment) %>%
    filter(phase == current_phase & analysis_type == current_analysis_type) %>%
    filter(invalid != "TRUE") %>%
    # need to do rowwise for the pluck
    rowwise() %>%
    mutate(frequencies = pluck(summary, "Freq (kHz)") %>% unique %>% str_flatten_comma()) %>%
    # filter to today's data based on analysis_type
    # Octave can not be limited to Freq
    {if (str_detect(unique(.$analysis_type), pattern = "Octave|Oddball", negate = TRUE)) filter(., any(frequencies %>% str_split(pattern = ", ", simplify = TRUE) %in% current_frequencies)) else .} %>%
    unnest_wider(stats)
  
  # Graph production --------------------------------------------------------
  # takes graph_data data frame (which has nested dfs) and produces 3 graphs -
  # one for dprime, one for reaction and one for hit%
  
  # Gap Detection
  if (str_detect(current_analysis_type, pattern = "Gap")) {
    # Calculate TH
    # There is no TH possible in Training files
    # In this case we should always only have 1 frequency but to future proof
    # I put in the code for multiple frequencies
    if (current_analysis_type != "Training - Gap") {
      if(length(current_frequencies) != 1) {
        TH = graph_data %>% filter(complete_block_count > 1) %>% 
          transmute(temp = map_dbl(threshold, ~ filter(., Freq == 4)$TH)) %>% 
          .$temp %>% mean(na.rm = TRUE)
      } else TH = mean(graph_data$threshold$TH, na.rm = TRUE)
    }
    
    ################
    # dprime graph
    ################
    what_to_graph = "dprime"
    x_column = "Dur"; y_column = "dprime"
    if (current_analysis_type == "Training - Gap") dprime_graph = Blank_Grapher(ggplot(graph_data))
    else {
      # Graph
      dprime_graph = graph_data %>%
        unnest(what_to_graph) %>%
        ggplot(aes(x = Dur, y = dprime)) %>%
        Line_Grapher
    }
    # Add axis labels
    dprime_graph = dprime_graph + labs(x = "Gap Duration", y = "d'")
    
    ################
    # Reaction graph
    ################
    what_to_graph = "reaction"
    x_column = "Dur (ms)"; y_column = "Rxn"
    # Graph
    rxn_graph = graph_data %>%
      unnest(what_to_graph) %>%
      ggplot(aes(x = `Dur (ms)`, y = Rxn)) 
    
    if (current_analysis_type == "Training - Gap") rxn_graph = Range_Grapher(rxn_graph)
      # you can remove non-relevant data by filtering rxn_graph$data but this
      # seems unnecessary and bad for day 1 of new stim
    else rxn_graph = Line_Grapher(rxn_graph)
    # Add axis labels
    rxn_graph = rxn_graph + labs(x = "Gap Duration", y = "Reaction Time")
    
    ################
    # Hit % graph
    ################
    # Need to add hit_detailed to do this

  }
  
  
  print(dprime_graph)
  print(rxn_graph)

  return(tibble_row(dprime_graph = dprime_graph, rxn_graph = rxn_graph))
}

ratID = NULL

# BBN
################
# # BBN Training
# rat_name = "TP6"; set_date = 20221017
# # BBN Single Duration
# rat_name = "Orange11", set_date = 20230225
# # BBN Mixed Duration
# rat_name = "Purple3"; set_date = 20230302

# Tones
################
# # Tones Training
# rat_name = "RP1"; set_date = 20221004
# # Tones Single Frequency
# rat_name = "RP1"; set_date = 20230224
# # Tones Mixed Frequency
# rat_name = "Orange4"; set_date = 20221029
# # Tones Mixed Frequency with BG
# rat_name = "Orange5"; set_date = 20221022

# Octave
################
# Octave Rxn time can not be standard as it has one intensity. Should be Freq vs. Rxn and then only on discrimination days
# on Training days, the graph needs to be against a range instead of a Line
# for dprime this requires unlisting reaction to get the Freq

# # Octave training
# rat_name = "GP1" 
# # Octave Discrimination
# TODO: untested because no runs in run_archives yet

# Oddball
################
# No dprime graph for Oddball

# # Tone, training, oddball
# rat_name = "GP5"; set_date = 20230303
# # Oddball Training (on trains)
# rat_name = "RP3"; set_date = 20230302
# # Oddball 
# rat_name = "Blue4"; set_date = 20230209
# # Oddball with BG
# TODO: untested because no runs in run_archives yet
# # Oddball with Catch
# rat_name = "Blue1"; set_date = 20220630

# Gap Detection
################
# # Gap Training
# rat_name = "Teal1"; ratID = 257; set_date = 20230303
# # Gap Rxn
# rat_name = "Red4"; ratID = 254; set_date = 20230303
# # Gap TH
# rat_name = "Red2"; ratID = 252; set_date = 20230222 # narrow TH window
# rat_name = "Red4"; ratID = 254; set_date = 20230301 # larger TH window

if(is_null(ratID)) ratID = rat_archive %>% filter(Rat_name == rat_name) %>% .$Rat_ID

# For testing purposes set_date is the date for graphing i.e. 'today'


Generate_Graph(rat_name = rat_name, ratID = ratID)

rm(list = c("ratID", "rat_name", "set_date"))