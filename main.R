Initialize <- function() {
   Load_Packages <- function() {
    # data loading/manipulation
    library(R.matlab);
    library(readxl); library(tidyverse); library(dplyr); library(tidyr)
    # Analysis
    library(psych); library(psycho);

    # Data visualization
    library(ggplot2); library(forcats);
  }

  Load_Packages()
  source("settings.R")  # hardcoded user variables
  setwd(settings$projects_folder)  # working directory from settings.R
  if (!exists("warnings_list")) warnings_list <<- list()  # initialize, if necessary, with empty list to record potential warnings_list
}

Import_Matlab <- function(current_mat_file) {
  Unlist_Matlab_To_Dataframe <- function(li) {
    return(t(apply(li, 1, unlist)) %>% as.data.frame())
  }

  Get_File_Name <- function() {
    file_name =
      trial_collection$name[1] %>%
      # remove excess info (i.e. .mat and then file location)
      stringr::str_remove(pattern = ".mat @ .*$", string = .)
    cat("Loading file:", file_name, sep = "\t", fill = TRUE)
    return(file_name)
  }

  Get_Stim_Master_List <- function() {
    Validate_Stim_Master_List <- function () {
      # Check that all stims are unique
      stim_not_unique = stim_master_list %>%
        dplyr::group_by(`Freq (kHz)`, `Inten (dB)`, `Dur (ms)`, `Type`) %>%
        dplyr::filter(n() > 1) %>%
        summarize(n = n(), .groups = 'drop')
      # Warning
      if (nrow(stim_not_unique) != 0) {
        warn = paste0("Action Required: Multiple (", nrow(stim_not_unique), ") identical stims in ", file_name)
        warning(paste0(warn, "\n"))
        warnings_list <<- append(warnings_list, warn)
      }

      only_one_frequency = length(unique(stim_master_list$`Freq (kHz)`)) == 1
      # Check StimSource against Frequency
      if (only_one_frequency) {
        freq_current = unique(stim_master_list$`Freq (kHz)`)
        source_current = unique(stim_master_list$`Stim Source`)

        if (xor(freq_current == 0, source_current == "BBN")) {
          warn = paste0("Action Required: Source (", source_current, ") does not match single Frequency (", freq_current, " kHz).")
          warning(paste0(warn, "\n"))
          warnings_list <<- append(warnings_list, warn)

          #TODO tomorrow - corrective action, and break warn into a helper function

          if (freq_current == 0) source_corrected = "BBN"
          else source_corrected = "tone"

          stim_master_list <<- stim_master_list %>%
            dplyr::mutate(`Stim Source` = stringr::str_replace(`Stim Source`, source_current, source_corrected))

          warn = paste0("Overriding Stim Source: Old (", source_current, ") to New (", source_corrected, ") based on single Frequency (", freq_current, " kHz).")
          warning(paste0(warn, "\n"))
          warnings_list <<- append(warnings_list, warn)
        }
      }
    }








    # create short (~50) table of all possible stimulus configurations (with delay specified as an allowed window)
    stim_master_list = trial_collection$source.list
    # remove sublists and transform into proper data frame
    # from: https://stackoverflow.com/questions/15930880/unlist-all-list-elements-in-a-dataframe
    stim_master_list =  Unlist_Matlab_To_Dataframe(stim_master_list)
    # add Column names
    names(stim_master_list) = append(unlist(trial_collection$stim.tag.list), "Repeat_number", after = 0)
    # fix column types to reflect contents (i.e. character -> numeric)
    stim_master_list = stim_master_list %>% dplyr::mutate_at(
        vars(Repeat_number, Type, `Freq (kHz)`, `Inten (dB)`, `Dur (ms)`, `Nose Out TL (s)`, `Time Out (s)`),
        ~as.numeric(.))
    # Add identifying number (for decoding)
    stim_master_list = dplyr::mutate(stim_master_list, "Stim_ID" = row_number())

    Validate_Stim_Master_List()
    return(stim_master_list)
  }


  Get_Run_Data <- function() {
    Get_Delay_DF <- function(run_data) {
      num_trials = nrow(run_data)
      delay_df = Unlist_Matlab_To_Dataframe(trial_collection$queue.list)[6]
      delay_df = head(delay_df, n = num_trials)
      names(delay_df) = "Delay (s)"
      return(delay_df)
    }

    # Verify that the file's summary table (final.result) agrees with counting the raw data directly
    Validate_Mat_Summary <- function() {
      # file's summary table (matlab's work)
      results_total_trials = current_mat_file$final.result[,,1]$go.trial.num[1] + current_mat_file$final.result[,,1]$no.go.trial.num[1]
      results_hits = current_mat_file$final.result[,,1]$hit.num[1]
      results_misses = current_mat_file$final.result[,,1]$miss.num[1]
      results_CR = current_mat_file$final.result[,,1]$CR.num[1]
      results_FA = current_mat_file$final.result[,,1]$FA.num[1]

      # summary calculated from actual df (R's work)
      total_trials <<- run_data %>% dplyr::count() %>% as.numeric()
      hits_calc <<- run_data %>% dplyr::filter(Response == "Hit") %>% dplyr::count() %>% as.numeric()
      misses_calc <<- run_data %>% dplyr::filter(Response == "Miss") %>% dplyr::count() %>% as.numeric()
      CRs_calc <<- run_data %>% dplyr::filter(Response == "CR") %>% dplyr::count() %>% as.numeric()
      FAs_calc <<- run_data %>% dplyr::filter(Response == "FA") %>% dplyr::count() %>% as.numeric()

      if (results_total_trials == 0 | stim_type == "train") {
        cat("Validate_Mat_Summary: Skipped (no summary)", sep = "\t", fill = TRUE)
      }
      else {
        # Check calculated stats against MATLAB summary stats
        if (total_trials != results_total_trials) stop(paste0("Trial count summary (", results_total_trials, ") does not match Data (", total_trials, ")."))
        if (hits_calc != results_hits) stop(paste0("Hit count summary (", results_hits, ") does not match Data (", hits_calc, ")."))
        if (misses_calc != results_misses) stop(paste0("Miss count summary (", results_misses, ") does not match Data (", misses_calc, ")."))
        if (CRs_calc != results_CR) stop(paste0("Correct rejection count summary (", results_CR, ") does not match Data (", CRs_calc, ")."))
        if (FAs_calc != results_FA) stop(paste0("False alarm count summary (", results_FA, ") does not match Data (", FAs_calc, ")."))
      }
    }


    run_data_encoded = data.frame(current_mat_file$result)

    # The MATLAB file has 2 extra columns for some unknown reason
    if (all(run_data_encoded[7:8] != "0")) {
      stop("What are these columns storing?")
    } else {
      run_data_encoded = run_data_encoded[1:6]
    }

    names(run_data_encoded) = list("Time_since_file_start_(s)", "Stim_ID", "Trial_type", "Attempts_to_complete", "Response", "Reaction_(s)")

    run_data_encoded = run_data_encoded %>%
      dplyr::mutate(Response = dplyr::case_when(Response == 1 ~ "Hit",
                                                Response == 2 ~ "Miss",
                                                Response == 3 ~ "FA",
                                                Response == 4 ~ "CR",
                                                TRUE ~ "ERROR"))

    run_data = dplyr::left_join(x = run_data_encoded,
                                y = dplyr::select(stim_master_list, -Repeat_number, -`Delay (s)`),
                                by = "Stim_ID", all.x = TRUE)
    run_data = dplyr::bind_cols(run_data, Get_Delay_DF(run_data))

        Validate_Mat_Summary()
    return(run_data)
  }

  Omit_Trials <- function() {
    # These are typically demos or could trials that are ruled invalid by an observer.

    Query_Omitted_Trials <- function() {
      # get trial numbers to omit (these will be in a CSV format)
      omit_list = "2, 32, 6"
      #TODO actually do a popup and get information

      # cleanse any spaces
      omit_list = gsub("[[:space:]]", "", omit_list)
      # split string into list
      omit_list = strsplit(omit_list,",")

      #TODO handle ranges

      return(omit_list)
    }

    Remove_Trials <- function(omit_list) {
      # filter lines out of data
      run_data_kept = run_data %>% dplyr::filter(!row_number() %in% omit_list[[1]])
      # get number of trials omitted
      omit_count = length(omit_list[[1]]) %>% as.numeric()
      # calculate expected trials
      Trials_expected = total_trials - omit_count
      # Calculate # of kept trials
      Trials_kept = dplyr::count(run_data_kept) %>% as.numeric()
      # check
      if (Trials_kept != Trials_expected) stop("Omitting Trials - expected kept count does not match")

      return(run_data_kept)
    }

    r = run_data
    omit_list = Query_Omitted_Trials()
    if (!is.null(omit_list)) r = Remove_Trials(omit_list)
    return(r)
  }

  Identify_Analysis_Type <- function() {
    # List of go sound frequencies
    # file_frequencies = unique(stim_master_list["Freq (kHz)"])

    # Get ranges for each frequency
    file_frequency_ranges = stim_master_list %>%
      dplyr::filter(`Inten (dB)` != -100) %>% # Remove No-Go from range
      dplyr::group_by(`Freq (kHz)`, `Delay (s)`, `Type`, `Repeat_number`) %>%
      dplyr::summarise(dB = unique(`Inten (dB)`), .groups = 'drop') # Get each unique dB


    Get_File_Summary_BBN_Tone <- function() {
      # Make summary data table with step size
      file_summary = file_frequency_ranges %>%
        dplyr::group_by(`Freq (kHz)`, `Delay (s)`, `Type`) %>%
        dplyr::summarise(dB_min = min(dB),
                         dB_max = max(dB),
                         dB_step_size = dB - lag(dB, default = first(dB)),
                         .groups = 'keep')
      # still grouped following this step, which is needed to remove the 1st row of each table that has a 0 step_size that is wrong for files with actual step_sizes

      if (!identical(file_summary$dB_min, file_summary$dB_max)) {
        file_summary = file_summary %>%
          dplyr::slice(-1) %>% # Drop 1st row of each sub-table
          .[!duplicated(.), ] # reduce to unique rows. Should be 1 row per frequency unless something is screwed up
      }

      # Check for mismatched step size
      if (length(unique(file_summary$dB_step_size)) != 1) {
        warn = paste0("Action Required: Mismatched step size (", unique(file_summary$dB_step_size), ") in file ", file_name)
        warning(paste0(warn, "\n"))
        warnings_list <<- append(warnings_list, warn)
      }
      return(file_summary)
    }

    Get_File_Summary_Oddball <- function() {
      # Make summary data table with step size
      go_freq = stim_master_list %>%
        filter(Type == 1) %>%
        .$`Freq (kHz)`

      go_number = stim_master_list %>%
        filter(Type == 1) %>%
        .$Stim_ID %>% as.character()

      spacer_number = stim_master_list %>%
        filter(Type == 0 & `Dur (ms)` >= settings$oddball_wait_min  &  `Dur (ms)` < settings$oddball_wait_max) %>%
        .$Stim_ID %>% as.character()

      nogo_freq = stim_master_list %>%
        filter(Type == 0 & `Dur (ms)` < settings$oddball_wait_min) %>%
        .$`Freq (kHz)`

      go_dB = stim_master_list %>%
        filter(Type == 1) %>%
        .$`Inten (dB)`

      nogo_dB = stim_master_list %>%
        filter(Type == 0 & `Dur (ms)` < settings$oddball_wait_min) %>%
        .$`Inten (dB)`


      go_position_range = stim_master_list %>%
        filter(`Train Setting` != "" & Repeat_number != 0 & Type != 0) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(Temp_Position = stringr::str_remove_all(`Train Setting`, paste0("[", spacer_number, " ]")), # strip characters from train so we can determine go tone position directly
                      Position = stringr::str_locate(pattern = go_number, Temp_Position)) %>% # positions of go tones
                      .$Position %>% as.data.frame() %>% .[,1] # str_locate gives start and stop positions, we just need either one

      catch = stim_master_list %>%
        filter(Type == 0 & Repeat_number > 0) %>%
        nrow() > 0

      odds = stim_master_list %>%
        filter(Type == 5) %>%
        .$Repeat_number %>% unique() %>%
        tibble(odds = ., position = go_position_range)


      file_summary = c(
        go_freq = go_freq,
        go_dB = go_dB,
        nogo_freq = nogo_freq,
        nogo_dB  = nogo_dB,
        go_position_start = min(go_position_range),
        go_position_stop = max(go_position_range),
        catch = catch,
        odds = odds
      )

      return(file_summary)
    }



    if (stim_type == "BBN" | stim_type == "tone") file_summary <<- Get_File_Summary_BBN_Tone()
    else if (stim_type == "train") file_summary <<- Get_File_Summary_Oddball()
    else stop(paste0("Unknown stim type: ", stim_type))

    analysis_type = NULL

    Check_Tonal <- function() {
      # Determine if it has custom ranges (i.e. not all frequencies have the same range)
      has_different_dB_ranges_for_frequencies = length(unique(file_summary$dB_min)) != 1 | length(unique(file_summary$dB_max)) != 1
      # Determine if octave file (in that case one of the normal intensity (dB) should be 0 or non-rewarded)
      # Note that for each type 1 and type 0 the min & max should be equal (i.e. one intensity) but not necessarily between type 1 & 0
      has_audible_NoGo = any(file_summary$Type == 0)
      # Test octave files have multiple types of No Go trials in the audible range
      has_more_than_one_NoGo = length(unique(dplyr::filter(file_summary, Type == 0)$`Freq (kHz)`)) > 1
      # Does the file have only a single frequency, for training?
      has_only_one_frequency = length(unique(stim_master_list$`Freq (kHz)`)) == 1

      # For tonal files (octaves, or mainly 4-32kHz)
      if (has_different_dB_ranges_for_frequencies) analysis_type <<- "custom tone"
      else if (has_audible_NoGo & has_more_than_one_NoGo) analysis_type <<- "octave"
      else if (has_audible_NoGo & !has_more_than_one_NoGo) analysis_type <<- "training octave"
      else if (has_only_one_frequency) analysis_type <<- "training tone"
      else if (!has_different_dB_ranges_for_frequencies) analysis_type <<- "standard tone"
      else (stop("Unknown tonal file type."))
    }

    Check_BBN <- function() {
      has_one_dB = file_summary$dB_min == file_summary$dB_max
      has_multiple_durations = length(unique(stim_master_list$`Dur (ms)`)) > 1

      # For broadband files (training or otherwise)
      # Determine if training
      if (has_one_dB) analysis_type <<- "BBN Training"
      else if (has_multiple_durations) analysis_type <<- "BBN duration"
      else (analysis_type = "BBN")
    }

    Check_Oddball <- function() {
      # Determine if catch trials
      has_catch_trials = file_summary$catch
      # Determine if even odds
      has_uneven_trial_odds = length(unique(file_summary$odds.odds)) > 1

      # For Oddball files (training or otherwise)
      if (has_catch_trials & has_uneven_trial_odds) analysis_type <<- "Oddball with odds & catch trials"
      else if (has_catch_trials) analysis_type <<- "Oddball with catch trials"
      else if (has_uneven_trial_odds) analysis_type <<- "Oddball with uneven trial odds"
      else if (!has_catch_trials & !has_uneven_trial_odds) analysis_type <<- "Oddball standard"
      else stop("Unknown Oddball file type.")
    }

    if (stim_type == "tone") Check_Tonal()
    if (stim_type == "BBN") Check_BBN()
    if (stim_type == "train") Check_Oddball()

    if (is.null("analysis_type")) stop("\nUnknown file type. Can not proceed with analysis")
    else (cat("Proceeding with analysis:", analysis_type, sep = "\t", fill = TRUE))

    return(analysis_type)
  }

  Build_Filename <- function() {
    Get_File_Settings <- function() {
      file_settings = trial_collection$para[,,1]

      # Background Type ---------------------------------------------------------
      if (is.na(file_settings$BG.sound[[1]][1]) == TRUE) {
        background_dB = "None"
        background_file = "None"
        background_type = "None"
      } else {
        background_dB = file_settings$BG.sound.inten[1]
        background_file = file_settings$BG.sound["filepath",,]$filepath["filename",,]$filename[1]
        background_type =
          background_file %>%
          stringr::str_remove(pattern = "^BG_", string = .) %>%
          stringr::str_remove(pattern = ".mat", string = .)
        background_type = switch(background_type,
                                 "PKN" = "Pink",
                                 "PNK" = "Pink",
                                 "WN" = "White",
                                 "BBN" = "Broadband",)
      }

      r = list(
        background_dB = background_dB,
        background_file = background_file,
        background_type = background_type,

        # Maximum number of back to back no go trials (0 or blank is infinite)
        no_go_max_touching = file_settings$no.go.trial.max.num[[1]],

        # settings from stim_master_list
        response_window = unique(stim_master_list["Nose Out TL (s)"]) %>% as.numeric(),
        lockout = unique(stim_master_list$`Time Out (s)`)[unique(stim_master_list$`Time Out (s)`) > 0],
        delay = unique(stim_master_list$`Delay (s)`),
        duration = unique(stim_master_list["Dur (ms)"]), # List of length of go sound - can be up to 3 values (50, 100, & 300) in our current file system

        # DTW setting in the behavior program or TR in file names
        # DTW = detect time window & TR = Trigger
        # In either case, this is not to be confused with the response window (above)
        #   i.e. how long the rat has post trial start to respond
        # This is how long the nose must remain out to be counted as 'withdraw' or
        # response. Response time is for the withdraw.
        trigger_sensitivity = file_settings$detect.time.win[[1]],

        # Nose light operational?
        nose_light = file_settings$nose.light[[1]] %>% as.logical()
      )
      if (length(r$response_window) > 1) stop("Multiple response windows:", r$response_window,". Aborting.")

      return(r)
    }


    Tonal <- function() {
      go_kHz_range = paste0(file_summary %>% dplyr::filter(Type == 1) %>% .$`Freq (kHz)` %>% min(), "-",
                            file_summary %>% dplyr::filter(Type == 1) %>% .$`Freq (kHz)` %>% max(), "kHz")

      go_dB_range = paste0(file_summary %>% dplyr::filter(Type == 1) %>% .$dB_min %>% unique(), "-",
                           file_summary %>% dplyr::filter(Type == 1) %>% .$dB_max %>% unique(), "dB")

      print(file_summary)
      print(go_kHz_range)
      print(go_dB_range)
      print(file_settings$duration)
      print(file_settings$lockout)

      has_Response_window = file_settings$response_window != 2
      has_TR = file_settings$trigger_sensitivity != 200
      has_BG = file_settings$background_type != "None"
      BG = if (has_BG) paste0(stringr::str_remove(pattern = ".mat", string = file_settings$background_file), "_", background_dB, "dB")

      if (has_Response_window & has_TR & has_BG) {
        computed_file_name =  paste0(go_kHz_range, "_", go_dB_range, "_", file_settings$duration, "ms_", file_settings$lockout, "s_", file_settings$response_window, "s_", "TR", file_settings$trigger_sensitivity, "ms_", BG)
      } else if (has_Response_window & has_TR ) {
        computed_file_name =  paste0(go_kHz_range, "_", go_dB_range, "_", file_settings$duration, "ms_", file_settings$lockout, "s_", file_settings$response_window, "s_", "TR", file_settings$trigger_sensitivity, "ms")
      } else if (has_Response_window) {
        computed_file_name =  paste0(go_kHz_range, "_", go_dB_range, "_", file_settings$duration, "ms_", file_settings$lockout, "s_", file_settings$response_window, "s")
      } else if (has_TR ) {
        computed_file_name =  paste0(go_kHz_range, "_", go_dB_range, "_", file_settings$duration, "ms_", file_settings$lockout, "s_", "TR", file_settings$trigger_senesitivity, "ms")
      }

      print(return(computed_file_name))
    }

    BBN <- function() {

    }

    Oddball <- function() {

    }





    file_settings = Get_File_Settings()

    if (stim_type == "tone") Tonal()
    else if (stim_type == "BBN") BBN()
    else if (stim_type == "train") Oddball()
    else stop("\nUnknown file type. Can not create filename.")
  }


# Import Workflow ---------------------------------------------------------
  trial_collection = current_mat_file$stim[,,1] # matlab object containing all (1000) trial configurations, and the parameters specified to generate individual trials
  file_name = Get_File_Name()
  stim_master_list <<- Get_Stim_Master_List()

  # manually calculate properties
  stim_block_size = sum(stim_master_list["Repeat_number"])
  stim_type = unique(stim_master_list["Stim Source"])


  if (nrow(stim_type) > 1) {
    if ("train" %in% stim_type$`Stim Source`) {
      stim_type = "train"
    }
    else stop(paste0("Multiple non-train stim types: ", stim_type))
  }
  else {
    stim_type = stim_type %>% as.character()
  }

  # load data from .mat file
  total_trials = hits_calc = misses_calc = CRs_calc = FAs_calc = NULL
  run_data <<- Get_Run_Data()

  # handle omitting trials
  run_data <<- Omit_Trials()

  # identify analysis type
  file_summary = NULL
  analysis_type <<- Identify_Analysis_Type()

  # filename
  Build_Filename()











}




# MAIN ---------------------------------------------------------
Initialize()
Import_Matlab(R.matlab::readMat(file.choose()))

