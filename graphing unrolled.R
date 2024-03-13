Generate_Graph <- function(rat_name, ratID) {
  # Thoughts ----------------------------------------------------------------
  # See graphing table for breakdown
  # TODO:
  # This code does not currently differentiate pre vs. post hearing loss/surgery/etc
  # You can handle this but it requires access to rat_archive, which I believe we have.
  
  # TODO:
  # It would be best if this code looked up the rat name for itself
  # It'd only need ratID that way, and could never be out of sync
  
  # Functions ---------------------------------------------------------------
  
  Range_Grapher <- function(plot) {
    graph_label = case_when(what_to_graph == "dprime" ~ "Sensitivity (d')",
                            .default = what_to_graph)
    
    # Range Graph for points
    plot +
      stat_summary(aes(color = "Average"),
                   fun = mean,
                   fun.min = function(x) mean(x) - sd(x),
                   fun.max = function(x) mean(x) + sd(x),
                   geom = "errorbar", linewidth = 10, width = 0) +
      geom_point(data = today_graph_data, aes(color = "Today", fill = "Today"), shape = 21, size = 5) +
      { if(current_task == "Training") {annotate(x = pluck(today_graph_data, paste0(what_to_graph, "$", x_column)),
                                                 y = pluck(today_graph_data, paste0(what_to_graph, "$", y_column)),
                                                 label = "Training", geom = "text", hjust = -0.5)} } +
      ggtitle(glue("{rat_name} {str_to_title(graph_label)} Range Check")) +
      scale_color_manual(values = c("Today" = "mediumslateblue", "Average" = "thistle"), name = "") +
      scale_fill_manual(values = c("Today" = "mediumslateblue", "Average" = "thistle"), name = "", guide = "none") +
      coord_cartesian(clip = "off") +
      theme_ipsum_es() +
      theme(legend.position = "bottom")
  }
  
  Line_Grapher <- function(plot) {
    graph_label = case_when(what_to_graph == "dprime" ~ "Sensitivity (d')",
                            .default = what_to_graph)
    
    # Line Graph for Curves
    plot +
      stat_summary(aes(color = "Average"),
                   fun = mean,
                   fun.min = function(x) mean(x) - sd(x),
                   fun.max = function(x) mean(x) + sd(x),
                   geom = "ribbon", linewidth = 2, fill = "thistle", show.legend = FALSE) +
      { if (exists("TH")) geom_vline(xintercept = TH, linetype = "dashed") } +
      { if (exists("TH")) annotate(x = TH, y = Inf, label = "Threshold", geom = "text", vjust = -0.5) } +
      { if (what_to_graph == "dprime") geom_hline(yintercept = 1.5, linetype = "dashed", color = "Blue") } +
      geom_line(data = today_graph_data, aes(color = "Today"), linewidth = 1.5) +
      geom_point(data = today_graph_data, shape=21, color="black", fill = "mediumslateblue", size = 5) +
      ggtitle(glue("{rat_name} {str_to_title(graph_label)} Curve Check")) +
      scale_color_manual(values = c("Today" = "mediumslateblue", "Average" = "thistle"), name = "") +
      scale_x_continuous(breaks = seq(from = -20, to = 100, by = 10)) +
      coord_cartesian(clip = "off") +
      theme_ipsum_es() +
      theme(legend.position = "bottom")
  }
  
  Blank_Grapher <- function(plot) {
    graph_label = case_when(what_to_graph == "dprime" ~ "Sensitivity (d')",
                            .default = what_to_graph)
    
    plot +
      xlim(0, 100) +
      ylim(0, 100) +
      ggtitle(glue("{rat_name} {str_to_title(graph_label)} does not exist")) +
      theme_ipsum_es() +
      theme(legend.position = "bottom")
  }
  
  # Graph Data generation ---------------------------------------------------
  rat_runs = run_archive %>% dplyr::filter(rat_ID == ratID)
  rat_runs = rat_runs %>% mutate(date_asDate = lubridate::ymd(date)) %>% arrange(desc(date))
  
  # Check to see if rat has had Hearing loss, surgery or any other procedure
  is_post_baseline = !is.na(filter(rat_archive, Rat_ID == ratID)$HL_date)
  
  # only want to graph relevant data so need today's data
  today_data = rat_runs %>%
    head(n = 1) # this should probably be a filter date, or like, select(max) or something.
  
  current_analysis_type = today_data$analysis_type
  current_phase = pluck(today_data, "assignment", 1, "phase")
  current_task = pluck(today_data, "assignment", 1, "task")
  current_detail = pluck(today_data, "assignment", 1, "detail")
  current_durations = pluck(today_data, "summary")[[Type = 1]]$duration[[1]]$`Dur (ms)` %>% unique %>% as.list()
  current_frequencies = pluck(today_data, "summary", 1, "Freq (kHz)") %>% unique %>% as.list()
  current_intensities = pluck(today_data, "summary", 1, "Inten (dB)") %>% unique %>% as.list()
  today_graph_data = today_data %>% unnest_wider(stats) %>% unnest(dprime) %>% unnest(reaction) %>% unnest(FA_detailed, names_sep = "_")
  
  # minimize data to mess with
  # need table with date, hit%, Frequency, Duration, dB, rxn, dprime
  graph_data =
    rat_runs %>%
    unnest_wider(assignment) %>%
    filter(phase == current_phase & analysis_type == current_analysis_type) %>%
    filter(invalid != "TRUE")
  
  #print(graph_data)
  
  # Remove unnecessary trial data due to being post-baseline
  if(is_post_baseline){
    cut_off_date = filter(rat_archive, Rat_ID == ratID)$HL_date
    # Select only runs since the procedure unless today is the 1st day following
    # the procedure (i.e. there is only 1 row which is today)
    filtered_graph_data = filter(graph_data, date > cut_off_date) %>%
      # We need to check that there are similar files to catch issues like its
      # the first time post-HL on BBN but not tones
      filter(analysis_type == current_analysis_type)
    if(nrow(filtered_graph_data) > 1) graph_data = filtered_graph_data
  }
  
  #print(graph_data)
  
  graph_data =
    graph_data %>%
    # need to do rowwise for the pluck
    rowwise() %>%
    mutate(frequencies = pluck(summary, "Freq (kHz)") %>% unique %>% str_flatten_comma()) %>%
    # filter to today's data based on analysis_type
    # Octave can not be limited to Freq
    {
      if (str_detect(unique(.$analysis_type), pattern = "Octave|Oddball", negate = TRUE))
        filter(., any(frequencies %>% str_split(pattern = ", ", simplify = TRUE) %in% current_frequencies))
      else
        .
    }
  
  
  #TODO confirm time save
  # graph_data =
  #   graph_data %>%
  #   group_by(date) %>%
  #   mutate(threshold = map_vec(stats, ~ .['threshold']),
  #          dprime = map_vec(stats, ~ .['dprime']),
  #          reaction = map_vec(stats, ~ .['reaction']))
  
  graph_data =
    graph_data %>%
    unnest_wider(stats)
  
  # Graph production --------------------------------------------------------
  # takes graph_data data frame (which has nested dfs) and produces 3 graphs -
  # one for dprime, one for reaction and one for hit%
  
  
  # Gap Detection -----------------------------------------------------------
  if (str_detect(current_analysis_type, pattern = "Gap")) {
    # Calculate TH
    # There is no TH possible in Training files
    # In this case we should always only have 1 frequency but to future proof
    # I put in the code to check for multiple frequencies
    if (current_analysis_type != "Training - Gap") {
      if(length(current_frequencies) == 1) TH = unnest(graph_data, threshold)$TH %>% mean(na.rm = TRUE)
      else stop("More than one frequency on a Gap file. This requires handling in the graphing code.")
    }
    
    ## dprime graph ##########
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
    dprime_graph = dprime_graph + 
      scale_x_continuous(breaks = seq(from = 0, to = 500, by = 50)) + 
      labs(x = "Gap Duration", y = "Sensitivity (d')")
    
    # Reaction graph ##########
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
    rxn_graph = rxn_graph + 
      scale_x_continuous(breaks = seq(from = 0, to = 500, by = 50)) + 
      labs(x = "Gap Duration", y = "Reaction Time (s)")
    
    # Hit % graph  ##########
    # Need to add hit_detailed to do this
    
  }
  
  
  # BBN ---------------------------------------------------------------------
  if (str_detect(current_analysis_type, pattern = "BBN")) {
    # Check for multiple durations in today's data
    has_multiple_durations = length(current_durations) > 1
    has_single_intensity = length(current_intensities) > 1
    
    if(has_multiple_durations & current_phase != "Edge Detection"){
      # Set today's data to 50 ms so only one of the multiple durations is graphed
      today_graph_data =  today_graph_data %>% filter(Dur == 50 & `Dur (ms)` == 50)
      
      # Select only Mixed duration files for comparison, unless today is the 1st
      # day of Mixed durations (i.e. there is only 1 row which is today)
      filtered_graph_data = filter(graph_data, detail == current_detail)
      if(nrow(filtered_graph_data) > 1) graph_data = filtered_graph_data
    } else if (has_multiple_durations & current_phase == "Edge Detection") {
      filtered_graph_data = filter(graph_data, str_detect(file_name,
                                                          pattern = glue("_{min(unlist(current_durations))}-{max(unlist(current_durations))}ms_")))
      if(nrow(filtered_graph_data) > 1) graph_data = filtered_graph_data
    } else {
      # Select only current duration files for comparison, unless today is the
      # 1st day of current duration (i.e. there is only 1 row which is today)
      filtered_graph_data = filter(graph_data, str_detect(file_name,
                                                          pattern = glue("_{current_durations}ms_")))
      if(nrow(filtered_graph_data) > 1) graph_data = filtered_graph_data
    }
    
    # Calculate TH
    # There is no TH possible in Training files
    # In the case of multiple durations, we take 50ms as this is the most restrictive
    if (current_analysis_type != "Training - BBN") {
      if(length(current_durations) > 1) {
        TH = graph_data %>% filter(complete_block_count > 1) %>%
          transmute(temp = map_dbl(threshold, ~ filter(., Dur == 50)$TH)) %>%
          .$temp %>% mean(na.rm = TRUE)
      } else TH = unnest(graph_data, threshold)$TH %>% mean(na.rm = TRUE)
    }
    
    
    ## dprime graph ##########
    what_to_graph = "dprime"
    x_column = "dB"; y_column = "dprime"
    if (current_analysis_type == "Training - BBN") dprime_graph = Blank_Grapher(ggplot(graph_data)) + labs(x = "Intensity (dB)", y = "Sensitivity (d')")
    else if (current_detail == "Single intensity") {
      ## Edge Detection ----------------
      # Graph
      dprime_graph = graph_data %>%
        unnest(all_of(what_to_graph)) %>%
        ggplot(aes(x = Dur, y = dprime)) %>%
        Line_Grapher +
        labs(x = "Dur (ms)", y = "Sensitivity (d')") +
        scale_x_continuous(breaks = c(seq(from = 0, to = 300, by = 50), seq(from = 400, to = 2000, by = 200))) 
    } else {
      # Graph
      dprime_graph = graph_data %>%
        unnest(all_of(what_to_graph)) %>%
        ggplot(aes(x = dB, y = dprime)) %>%
        Line_Grapher +
        labs(x = "Intensity (dB)", y = "Sensitivity (d')")
    }
    # Add axis labels
    # dprime_graph = dprime_graph + labs(x = "Intensity (dB)", y = "Sensitivity (d')")
    
    # Reaction graph ##########
    what_to_graph = "reaction"
    if (current_detail == "Single intensity") {
      ## Edge Detection ----------------
      # Graph
      x_column = "Dur (ms)"; y_column = "Rxn"
      # Graph
      rxn_graph = graph_data %>%
        unnest(what_to_graph) %>%
        ggplot(aes(x = `Dur (ms)`, y = Rxn)) %>%
        Line_Grapher +
        scale_x_continuous(breaks = c(seq(from = 0, to = 300, by = 50), seq(from = 400, to = 2000, by = 200)))  + 
        labs(x = "Duration (ms)", y = "Reaction Time (s)")
    } else {
    x_column = "Inten (dB)"; y_column = "Rxn"
    
    if (current_analysis_type == "Training - BBN") {
      rxn_graph =  graph_data %>%
        unnest(all_of(what_to_graph)) %>%
        ggplot(aes(x = `Inten (dB)`, y = Rxn)) %>%
        Range_Grapher + 
        labs(x = "Intensity (dB)", y = "Reaction Time (s)")
    } else {
    # Graph
    rxn_graph = graph_data %>%
      unnest(all_of(what_to_graph)) %>%
      ggplot(aes(x = `Inten (dB)`, y = Rxn)) %>%
      Line_Grapher +
      labs(x = "Intensity (dB)", y = "Reaction Time (s)")
    }}
    
    # Hit % graph  ##########
    # Need to add hit_detailed to do this
    
  }
  
  
  # Tones ---------------------------------------------------------------------
  if (str_detect(current_analysis_type, pattern = "Tone")) {
    # Check for multiple durations in today's data
    has_multiple_frequencies = length(current_frequencies) > 1
    
    if(has_multiple_frequencies){
      # Set today's data to 8kHz so only one of the multiple frequencies is graphed
      today_graph_data =  today_graph_data %>% filter(Freq == 8 & `Freq (kHz)` == 8)
    }
    
    # Calculate TH
    # There is no TH possible in Training files
    # In the case of multiple durations, we take 50ms as this is the most restrictive
    if (current_analysis_type != "Training - Tone") {
      if(length(current_durations) > 1) {
        TH = graph_data %>% filter(complete_block_count > 1) %>%
          transmute(temp = map_dbl(threshold, ~ filter(., Dur == 50)$TH)) %>%
          .$temp %>% mean(na.rm = TRUE)
      } else TH = unnest(graph_data, threshold)$TH %>% mean(na.rm = TRUE)
    }
    
    
    ## dprime graph ##########
    what_to_graph = "dprime"
    x_column = "dB"; y_column = "dprime"
    if (current_analysis_type == "Training - Tone") dprime_graph = Blank_Grapher(ggplot(graph_data))
    else {
      # Graph
      dprime_graph = graph_data %>%
        unnest(what_to_graph) %>%
        ggplot(aes(x = dB, y = dprime)) %>%
        Line_Grapher
    }
    # Add axis labels
    dprime_graph = dprime_graph + labs(x = "Intensity (dB)", y = "Sensitivity (d')")
    
    # Reaction graph ##########
    what_to_graph = "reaction"
    x_column = "Inten (dB)"; y_column = "Rxn"
    # Graph
    rxn_graph = graph_data %>%
      unnest(what_to_graph) %>%
      ggplot(aes(x = `Inten (dB)`, y = Rxn))
    
    if (current_analysis_type == "Training - Tone") rxn_graph = Range_Grapher(rxn_graph)
    # you can remove non-relevant data by filtering rxn_graph$data but this
    # seems unnecessary and bad for day 1 of new stim
    else rxn_graph = Line_Grapher(rxn_graph)
    # Add axis labels
    rxn_graph = rxn_graph + labs(x = "Intensity (dB)", y = "Reaction Time (s)")
    
    
    # Hit % graph  ##########
    # Need to add hit_detailed to do this
    
  }
  
  # Octave ---------------------------------------------------------------------
  # TODO: untested for discrimination, note this will use Detailed FA I believe
  if (str_detect(current_analysis_type, pattern = "Octave")) {
    # filter to only the same detail (i.e. Normal or reverse) as we don't need the other
    graph_data = filter(graph_data, detail == current_detail)
    
    # If octave discrimination (i.e. has steps), convert to octave steps rather than raw frequency
    has_multiple_frequencies = length(current_frequencies) > 2
    
    if(has_multiple_frequencies){
      # only go is consistent as the number of no-gos fluctuates
      go_kHz = filter(today_graph_data$summary[[1]], Type == 1)$`Freq (kHz)`
      
      # Add octave columns: octave_fraction is the fraction of the octave,
      # octave_step is the nearest 16th step (i.e. x/16)
      graph_data =
        graph_data %>%
        mutate(FA_detailed = map(FA_detailed, ~ mutate(., octave_fraction = log(go_kHz/`Freq (kHz)`)/log(2),
                                                       octave_step = abs(round(octave_fraction * 16)))))
    }
    
    # Calculate TH
    # None, but could consider calculating where more like go than no-go i.e. > 50% FA%
    
    
    ## dprime graph ##########
    what_to_graph = "dprime"
    x_column = "FA_detailed_Freq (kHz)"; y_column = "FA_detailed_dprime"
    if (current_analysis_type == "Training - Octave") dprime_graph = Blank_Grapher(ggplot(graph_data))
    else {
      # Graph
      dprime_graph = graph_data %>%
        unnest(FA_detailed, names_sep = "_") %>%
        ggplot(aes(x = `FA_detailed_Freq (kHz)`, y = FA_detailed_dprime)) %>%
        Line_Grapher
    }
    # Add axis labels
    dprime_graph = dprime_graph + labs(x = "Frequency (kHz)", y = "Sensitivity (d')")
    
    # Reaction graph ##########
    what_to_graph = "reaction"
    x_column = "Inten (dB)"; y_column = "Rxn"
    # Graph
    rxn_graph = graph_data %>%
      unnest(what_to_graph) %>%
      ggplot(aes(x = `Inten (dB)`, y = Rxn))  %>%
      Range_Grapher
    # Add axis labels
    rxn_graph = rxn_graph + labs(x = "Intensity (dB)", y = "Reaction Time (s)")
    
    
    # FA % graph  ##########
    what_to_graph = "False Alarm"
    x_column = "FA_detailed_Freq (kHz)"; y_column = "FA_detailed_FA_percent_detailed"
    if (current_analysis_type == "Training - Octave") hit_graph = Blank_Grapher(ggplot(graph_data))
    else {
      today_graph_data = mutate(today_graph_data, FA_detailed_FA_percent_detailed = FA_detailed_FA_percent_detailed * 100)
      
      # Graph
      hit_graph = graph_data %>%
        unnest(FA_detailed, names_sep = "_") %>%
        mutate(FA_detailed_FA_percent_detailed = FA_detailed_FA_percent_detailed * 100) %>%
        ggplot(aes(x = `FA_detailed_Freq (kHz)`, y = FA_detailed_FA_percent_detailed)) %>%
        Line_Grapher
    }
    # Add axis labels
    hit_graph = hit_graph + labs(x = "Frequency (kHz)", y = "False Alarm %")
    
  }
  
  # Oddball ---------------------------------------------------------------------
  # TODO: untested for discrimination, note this will use Detailed FA I believe
  if (str_detect(current_analysis_type, pattern = "Oddball")) {
    # Calculate TH
    # None
    
    ## dprime graph ##########
    # Does not exist but could do detailed FA graph instead
    what_to_graph = "dprime"
    x_column = "dB"; y_column = "dprime"
    dprime_graph = Blank_Grapher(ggplot(graph_data))
    # Add axis labels
    dprime_graph = dprime_graph + 
      scale_x_continuous(breaks = seq(from = 1, to = 10, by = 1))
    labs(x = "Intensity (dB)", y = "Sensitivity (d')")
    
    
    # Reaction graph ##########
    what_to_graph = "reaction"
    x_column = "Inten (dB)"; y_column = "Rxn"
    # Graph
    rxn_graph = graph_data %>%
      unnest(what_to_graph) %>%
      ggplot(aes(x = `Inten (dB)`, y = Rxn))
    
    if (current_analysis_type == "Training - Oddball") rxn_graph = Range_Grapher(rxn_graph)
    # you can remove non-relevant data by filtering rxn_graph$data but this
    # seems unnecessary and bad for day 1 of new stim
    else rxn_graph = Line_Grapher(rxn_graph)
    # Add axis labels
    rxn_graph = rxn_graph + 
      scale_x_continuous(breaks = seq(from = 1, to = 10, by = 1)) + 
      labs(x = "Intensity (dB)", y = "Reaction Time (s)")
    
    
    # Hit % graph  ##########
    # Need to add hit_detailed to do this
    
  }
  
  # Duration Testing ---------------------------------------------------------------------
  if (str_detect(current_analysis_type, pattern = "Duration Testing")) {
    # Check for multiple durations in today's data
    has_multiple_frequencies = length(current_frequencies) > 1
    
    if(has_multiple_frequencies){
      # Set today's data to 8kHz so only one of the multiple frequencies is graphed
      today_graph_data =  today_graph_data %>% filter(Freq == 8 & `Freq (kHz)` == 8)
    }
    
    ## dprime graph ##########
    what_to_graph = "dprime"
    x_column = "Dur (ms)"; y_column = "dprime"
    if (current_phase == "Edge Detection") {
      ## Edge Detection ----------------
      # Graph
      dprime_graph = graph_data %>%
        unnest(all_of(what_to_graph)) %>%
        ggplot(aes(x = Dur, y = dprime)) %>%
        Line_Grapher +
        labs(x = "Dur (ms)", y = "Sensitivity (d')") +
        scale_x_continuous(breaks = c(seq(from = 0, to = 300, by = 50), seq(from = 400, to = 2000, by = 200))) 
    } else {
      ## Single Intensity ----------------
      dprime_graph = Blank_Grapher(ggplot(graph_data))
    }
    
    
    
    ## Reaction graph ##########
    what_to_graph = "reaction"
    x_column = "Dur (ms)"; y_column = "Rxn"
    # Graph
    rxn_graph = graph_data %>%
      unnest(what_to_graph) %>%
      ggplot(aes(x = `Dur (ms)`, y = Rxn)) %>%
      Line_Grapher +
      scale_x_continuous(breaks = c(seq(from = 0, to = 300, by = 50), seq(from = 400, to = 2000, by = 200)))  + 
      labs(x = "Duration (ms)", y = "Reaction Time (s)")
    
  }
  
  # print(rxn_graph)
  
  return(tibble_row(dprime_graph = dprime_graph, rxn_graph = rxn_graph))
}
