Generate_Graph <- function(rat_name, ratID) {
  # Thoughts ----------------------------------------------------------------
  # See graphing table for breakdown
  # ISSUE: BBN & Tone mixed would have 3 lines that have similar ranges.
  #        Therefore, I will graph 50ms for Mixed BBN and 8kHz for Mixed Tones.
  
  # Functions ---------------------------------------------------------------
  
  Range_Grapher <- function(df) {
    # Range Graph for points
     df %>%
      ggplot(aes(x = get(x_column), y = get(y_column))) +
      stat_summary(aes(color = "Average"),
                   fun = mean,
                   fun.min = function(x) mean(x) - sd(x),
                   fun.max = function(x) mean(x) + sd(x),
                   geom = "errorbar", linewidth = 10, width = 0) +
      geom_point(data = today_data, shape = 21, color = "black", fill = "mediumslateblue", size = 5) +
      # Produces warning about group aesthetic b/c only 1 but needed for nice legend
      geom_line(data = today_data, aes(color = "Today"), linewidth = 1.5) +
      { if(current_task == "Training") {annotate(x = pluck(today_data, paste(x_column)), y = pluck(today_data, paste(y_column)), label = "Training", geom = "text", hjust = -0.5)} } +
      ggtitle(glue("{rat_name} {what_to_graph} Range Check")) +
      scale_color_manual(values = c("Today" = "mediumslateblue", "Average" = "thistle"), name = "") +
      coord_cartesian(clip = "off") +
      theme_ipsum_es() +
      theme(legend.position = "bottom") +
      labs(x = paste(x_column), y = paste(y_column))
  }
  
  Line_Grapher <- function(df) {
    # Line Graph for Curves
    df %>%
      ggplot(aes(x = get(x_column), y = get(y_column))) +
      stat_summary(aes(color = "Average"),
                   fun = mean,
                   fun.min = function(x) mean(x) - sd(x),
                   fun.max = function(x) mean(x) + sd(x),
                   geom = "ribbon", linewidth = 2, fill = "thistle", show_guide=FALSE) +
      { if (exists("TH")) geom_vline(xintercept = TH, linetype = "dashed") } +
      { if (exists("TH")) annotate(x = TH, y = Inf, label = "Threshold", geom = "text", vjust = -0.5) } +
      geom_line(data = today_data, aes(color = "Today"), linewidth = 1.5) +
      geom_point(data = today_data, shape=21, color="black", fill = "mediumslateblue", size = 5) +
      ggtitle(glue("{rat_name} {what_to_graph} Curve Check")) +
      scale_color_manual(values = c("Today" = "mediumslateblue", "Average" = "thistle"), name = "") +
      coord_cartesian(clip = "off") +
      theme_ipsum_es() +
      theme(legend.position = "bottom") +
      labs(x = paste(x_column), y = paste(y_column))
  }
  
  # Workflow ----------------------------------------------------------------
  
  
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
  # Treat Tones, Training, Oddball as "Training - Octave"
  if(current_task == "Training" && current_detail == "Oddball") current_analysis_type = "Training - Octave"
  current_frequencies = pluck(today_data, "summary", 1, "Freq (kHz)") %>% unique %>% as.list()
  today_data = today_data %>% unnest_wider(stats) %>% unnest(what_to_graph)
  
  # minimize data to mess with
  # need table with date, hit%, Frequency, Duration, dB, rxn, dprime 
  graph_data =
    rat_runs %>%
    # Due to bad data
    filter(! date %in% c("20220107", "20220106", "20220105", "20220104", "20220103")) %>%
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
    # # Omit days with > 45% FA, i.e. guessing
    # filter(FA_percent < user_settings$FA_cutoff) %>%
  
  # Check to see if we can calculate a TH
  # No TH for: Training - Gap, Training - BBN, Training - Tone, Training - Octave, Training - Oddball, Oddball (Standard), Oddball (Catch) 
  if (current_analysis_type %in% c("Tone (Single)", "BBN Mixed Duration", "BBN (Standard)", "Gap (Standard)", "Tone (Standard)", "Tone (Thresholding)")) {
    if(length(current_frequencies) != 1){
      TH = graph_data %>% filter(complete_block_count > 1) %>% transmute(temp = map_dbl(threshold, ~ filter(., Freq == 4)$TH)) %>% .$temp %>% mean(na.rm = TRUE)
        
    } else TH = mean(graph_data$threshold$TH, na.rm = TRUE)
  } else {
    dprime = FALSE
    if (what_to_graph == "dprime") {
      writeLines("You can't make a dprime graph because dprime is non-sensical")
      graph_data = NULL
    }
  }
  

# Graph production --------------------------------------------------------
  graph = NULL
  
  if(is_empty(graph_data)) {
    graph =
      ggplot(graph_data) + 
      geom_point() + 
      xlim(0, 100) + 
      ylim(0, 100) + 
      ggtitle(glue("{rat_name} {what_to_graph} does not exist")) +
      theme_ipsum_es() +
      theme(legend.position = "bottom")
    
    return(graph)
  } else {
    names_list = graph_data %>% unnest(what_to_graph) %>% names
    
    # Check Experiment
    if (str_detect(current_analysis_type, pattern = "Gap")) {
      x_column = names_list[[str_which(names_list, pattern = "Dur")]]
    } else if (str_detect(current_analysis_type, pattern = "(Tone|BBN|Oddball)")) {
      x_column = names_list[[str_which(names_list, pattern = "dB")]]
    } else if (str_detect(current_analysis_type, pattern = "Octave")) {
      x_column = names_list[[str_which(names_list, pattern = "Freq")]]
    } else {
      stop("ERROR: unreognized analysis type of ", current_analysis_type)
    }
    
    
    
    if (what_to_graph == "dprime") {
      y_column = "dprime"
    } else if  (what_to_graph == "reaction") {
      y_column = "Rxn"
    } else if  (what_to_graph == "hit") {
      y_column = "hit_percent"
    }
    
    if (current_analysis_type == "Training - Octave") {
      graph = 
        graph_data %>% 
        unnest(what_to_graph) %>%
        unnest(reaction) %>%
        Range_Grapher
    
    } else if (str_detect(current_analysis_type, pattern = "Training")) {
      # Note this must be after the Octave Training tester
      
      graph = 
        graph_data %>% 
        unnest(what_to_graph) %>%
        Range_Grapher
      
    } else if (current_phase == "Gap Detection") {
      # check for the need to zoom in on a region...note that this prevents the TH line from being labeled
      has_small_min_range = max(today_data$`Dur (ms)`) - min(today_data$`Dur (ms)`) < 50
      
      graph = 
        graph_data %>% 
        unnest(what_to_graph) %>%
        Line_Grapher 
      
      # TODO: Test this code
      if (has_small_min_range) {
        # will complain about overwriting coordinate system - this is expected
        graph = graph +
          coord_cartesian(xlim = c(today_data %>% select(x_column) %>% min(na.rm = TRUE),
                                   today_data %>% select(x_column) %>% max(na.rm = TRUE)),
                          ylim = c(today_data %>% select(y_column) %>% min(na.rm = TRUE) * 0.9,
                                   today_data %>% select(y_column) %>% max(na.rm = TRUE) * 1.1))
      }
    } else {
      #TODO Add check for list in x_column then filter
      
      graph = 
        graph_data %>% 
        unnest(what_to_graph) %>%
        Line_Grapher 
      
    }
    
    if (is_null(graph)) {
      # This should be impossible to hit given that the previous cascade ends in an else as a catchall
      # Set_date should be changed to Sys.Date
      writeLines(glue("ERROR: No {what_to_graph} graph made for {rat_name} ({ratID}) on {set_date} for an analysis of {current_analysis_type}"))
      graph =
        ggplot(graph_data) + 
        geom_point() + 
        xlim(0, 100) + 
        ylim(0, 100) + 
        ggtitle(glue("{rat_name} {what_to_graph} does not exist")) +
        theme_ipsum_es() +
        theme(legend.position = "bottom")
      
      return(graph)
    }
  
  return(graph)
  } 
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
rat_name = "RP1"; set_date = 20221004
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
# # Gap Rxn
# rat_name = "Red4"; ratID = 254; set_date = 20230303
# # Gap TH
# rat_name = "Red2"; ratID = 252; set_date = 20230222 # narrow TH window
# rat_name = "Red4"; ratID = 254; set_date = 20230301 # larger TH window

if(is_null(ratID)) ratID = rat_archive %>% filter(Rat_name == rat_name) %>% .$Rat_ID

# Can be hits, dprime or reaction
what_to_graph = "reaction"

# For testing purposes set_date is the date for graphing i.e. 'today'


Generate_Graph(rat_name = rat_name, ratID = ratID) %>% print

rm(list = c("ratID", "rat_name", "what_to_graph", "set_date"))
