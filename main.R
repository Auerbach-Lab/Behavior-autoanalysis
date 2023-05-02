
InitializeMain <- function() {
  Load_Packages <- function() {
    # data loading external file formats
    library(R.matlab); library(data.table);

    # data manipulation
    library(tidyverse); library(dplyr); library(tidyr); library(rlang); library(stringr); library(purrr); library(lubridate); library(glue);

    # analysis & visualization
    library(psycho); library(ggplot2); library(hrbrthemes); library(shiny);
  }

  Load_Packages()
  options(warn = 1) # we want to display warnings as they occur, so that it's clear which file caused which warnings
  source(paste0(projects_folder, "settings.R"))  # user variables

  rat_archive <<- fread(paste0(projects_folder, "rat_archive.csv"), na.strings = c("N/A","NA"))
  load(paste0(projects_folder, "run_archive.Rdata"), .GlobalEnv)
}

Process_File <- function(file_to_load, name, weight, observations, exclude_trials = "", old_file = FALSE, ignore_name_check = FALSE, use_shiny = FALSE, file_name_override = NULL, scientist = "", weightProblem = "", rxnProblem = "") {
  Import_Matlab <- function(file_to_load) {
    Unlist_Matlab_To_Dataframe <- function(li) {
      return(t(apply(li, 1, unlist)) %>% as.data.frame())
    }

    Get_Stim_Encoding_Table <- function() {
      Validate_Stim_Encoding_Table <- function () {
        # Check that all stims are unique
        stim_not_unique = stim_encoding_table %>%
          dplyr::group_by(`Freq (kHz)`, `Inten (dB)`, `Dur (ms)`, `Type`) %>%
          dplyr::filter(n() > 1) %>%
          summarize(n = n(), .groups = 'drop')
        # Warning
        if (nrow(stim_not_unique) != 0) {
          warn = paste0("ACTION REQUIRED: Multiple (", nrow(stim_not_unique), ") identical stims.")
          warning(paste0(warn, "\n"))
          warnings_list <<- append(warnings_list, warn)
        }

        only_one_frequency = length(unique(stim_encoding_table$`Freq (kHz)`)) == 1
        # Check StimSource against Frequency
        if (only_one_frequency) {
          freq_current = unique(stim_encoding_table$`Freq (kHz)`)
          source_current = unique(stim_encoding_table$`Stim Source`)

          # Check for gap or BBN matching frequncy 0
          if (xor(freq_current == 0, source_current %in% c("BBN", "gap"))) {
            warn = paste0("ACTION REQUIRED: Source (", source_current, ") does not match single Frequency (", freq_current, " kHz).")
            warning(paste0(warn, "\n"))
            warnings_list <<- append(warnings_list, warn)

            if (freq_current == 0) source_corrected = "BBN"
            else source_corrected = "tone"
            stim_encoding_table <<- stim_encoding_table %>%
              dplyr::mutate(`Stim Source` = stringr::str_replace(`Stim Source`, source_current, source_corrected))
            warn = paste0("Overriding Stim Source: Old (", source_current, ") to New (", source_corrected, ") based on single Frequency (", freq_current, " kHz).")
            warning(paste0(warn, "\n"))
            warnings_list <<- append(warnings_list, warn)
          }
        }
      }

      # create short (~50) table of all possible stimulus configurations (with delay specified as an allowed window)
      stim_encoding_table = trial_collection$source.list
      # remove sublists and transform into proper data frame
      # from: https://stackoverflow.com/questions/15930880/unlist-all-list-elements-in-a-dataframe
      stim_encoding_table =  Unlist_Matlab_To_Dataframe(stim_encoding_table)
      # add Column names
      names(stim_encoding_table) = append(unlist(trial_collection$stim.tag.list), "Repeat_number", after = 0)
      # fix column types to reflect contents (i.e. character -> numeric)
      stim_encoding_table = stim_encoding_table %>% dplyr::mutate_at(
          vars(Repeat_number, Type, `Freq (kHz)`, `Inten (dB)`, `Dur (ms)`, `Nose Out TL (s)`, `Time Out (s)`),
          ~as.numeric(.))
      # Add identifying number (for decoding)
      stim_encoding_table = dplyr::mutate(stim_encoding_table, "Stim_ID" = row_number())

      Validate_Stim_Encoding_Table()
      return(stim_encoding_table)
    }

    Get_Run_Properties <- function() {
      Get_Stim_Filename <- function() {
        r =
          trial_collection$name[1] %>%
          # remove excess info (i.e. .mat and then file location)
          stringr::str_remove(pattern = ".mat @ .*$", string = .)
        if (use_shiny) shiny::showNotification(glue("{r}")) #TODO replace default with shinyfeedback colored ones
        cat("Stim file:", r, sep = "\t", fill = TRUE)
        return(r)
      }

      Get_Rat_Name <- function() {
        if (is.null(file_name_override)) {
        # greedy group: (.*) to strip off as much as possible
        # then the main capture group which contains
          # lookbehind for either \ (escaped once because R, and then again cause regex, to \\\\) or / character, specified length 1 because r: (?<=[\\/]{1})
          # important! \\\\ is specific to R, for testing this pattern in e.g. RegExr you have to use \\ but remember to change it back to \\\\ for r!
          # capture of the rat name, lazy to avoid underscores: .+?
          # lookahead for a _: (?=_)

          pattern = "(.*)((?<=[\\\\/]{1}).+?(?=_))"
          filename = file_to_load
        } else {
          # shiny app provides a path to a temporary file and renames the file itself, so we have to work around that behavior
          # we could alternatively use the system file loader, but that has other downsides - it's difficult to get it to remember the current folder, and it doesn't give feedback of a file selected
          pattern = "^.+?(?=_)" # beginning up to underscore, lazy
          filename = file_name_override
          ignore_name_check = TRUE
        }
        #print(filename)

        r = stringr::str_match_all(filename, pattern = pattern) %>%
          unlist(recursive = TRUE) %>%
          tail (n = 1)

        r_compare = r %>%
          str_replace_all(" ", "") %>%
          str_to_lower()

        if (!ignore_name_check) {
          name_compare = name %>% str_replace_all(" ", "") %>% str_to_lower() # from undergraduate.R
        }

        if (rlang::is_empty(r)) stop("ERROR: system filename improper: ", filename)
        if (!ignore_name_check && r_compare != name_compare) stop(paste0("ABORT: Rat name given (", name_compare, ") does not match chosen file (", r_compare, ")."))

        return(r)
      }

      Get_Box_Number <- function() {
        if (is.null(file_name_override)) {
          filename = file_to_load
        } else {
          filename = file_name_override
        }

        # greedy group: (.*) to strip off as much as possible
        # then the main capture group which contains
        # lookbehind for a _BOX#: (?<=_BOX#)
        # capture of the box number: [:digit:]+
        # lookahead for the extension: (?=.mat)
        r = stringr::str_match_all(filename, pattern="(.*)((?<=_BOX#)[:digit:]+(?=.mat))") %>%
          unlist(recursive = TRUE) %>%
          tail (n = 1) %>%
          as.numeric()
        if(rlang::is_empty(r)) stop("ERROR: system filename improper: ", filename)
        return(r)
      }


      Get_Delay_Range <- function() {
        if (stim_type == "train") {
          r = stim_encoding_table %>% dplyr::filter(Repeat_number > 0) %>% .$`Delay (s)` %>% unique()
        } else {
          r = unique(stim_encoding_table$`Delay (s)`)
        }
        if (length(r)>1) {
          warn = paste0("ACTION REQUIRED: Multiple delay windows (", r, ").")
          warning(paste0(warn, "\n"))
          warnings_list <<- append(warnings_list, warn)
        }
        return(r)
      }

      run_properties = trial_collection$para[,,1]

      # Background Type
      if (is.na(run_properties$BG.sound[[1]][1]) == TRUE) {
        background_dB = "None"
        background_file = "None"
        background_type = "None"
      } else {
        background_dB = run_properties$BG.sound.inten[1]
        background_file = run_properties$BG.sound["filepath",,]$filepath["filename",,]$filename[1]
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

      stim_type = unique(stim_encoding_table["Stim Source"])
      if (nrow(stim_type) > 1) {
        if ("train" %in% stim_type$`Stim Source`) {
          stim_type = "train"
        }
        # if multiple types of go stimulus warn because mutliple types are excpected for oddball training but it should be go/nogo
        else if (nrow(filter(stim_encoding_table, Type == 1)) > 1) {
          warn = paste0("WARNING: Multiple non-oddball stim types: ", stim_type)
          warning(paste0(warn, "\n"))
          warnings_list <<- append(warnings_list, warn)
          stim_type = stim_encoding_table %>% filter(Type == "1") %>% .$`Stim Source`
        } else {stim_type = stim_encoding_table %>% filter(Type == "1") %>% .$`Stim Source`}
      } else {
        stim_type = stim_type %>% as.character()
      }

      r = list(
        rat_name = Get_Rat_Name(),
        box = Get_Box_Number(),

        stim_filename = Get_Stim_Filename(),
        stim_block_size = sum(stim_encoding_table["Repeat_number"]),
        stim_type = stim_type,

        background_dB = background_dB,
        background_file = background_file,
        background_type = background_type,

        # Maximum number of back to back no go trials (0 or blank is infinite)
        nogo_max_touching = ifelse(is_empty(run_properties$no.go.trial.max.num), "0",
                                    run_properties$no.go.trial.max.num[[1]]),

        # settings from run_properties$stim_encoding_table
        lockout = unique(stim_encoding_table$`Time Out (s)`)[unique(stim_encoding_table$`Time Out (s)`) > 0],
        delay = Get_Delay_Range(),
        duration = unique(stim_encoding_table["Dur (ms)"]), # List of length of go sound - can be up to 3 values (50, 100, & 300) in our current file system

        # DTW setting in the behavior program or TR in file names
        # DTW = detect time window & TR = Trigger
        # In either case, this is not to be confused with the response window (above)
        #   i.e. how long the rat has post trial start to respond
        # This is how long the nose must remain out to be counted as 'withdraw' or
        # response. Response time is for the withdraw.

        trigger_sensitivity = `if`(all(is.na(run_properties$detect.time.win)),   #if dtw is undefined, use 200
                                   200,
                                   run_properties$detect.time.win[[1]] %>% as.numeric()),

        # Nose light operational?
        nose_light = run_properties$nose.light[[1]] %>% as.logical(),

        stim_encoding_table = stim_encoding_table,

        #hoist the datestamp out of the loaded .mat file
        creation_time = current_mat_file$log[[2]][2] %>% unlist
      )

      filename_TR = stringr::str_extract(r$stim_filename,"_TR[:digit:]+ms") %>% str_extract("[:digit:]+") %>% as.numeric()
      if (!is.na(filename_TR)) if (filename_TR != r$trigger_sensitivity) {
        warn = paste0("ACTION REQUIRED: Mismatched trigger window in filename (", filename_TR, ") and user_settings (", r$trigger_sensitivity, ").")
        warning(paste0(warn, "\n"))
        warnings_list <<- append(warnings_list, warn)

        if (r$trigger_sensitivity != 200) {
          newname = stringr::str_replace(r$stim_filename,"_TR[:digit:]+ms",paste0("_TR", r$trigger_sensitivity, "ms"))
        }
        else {
          newname = stringr::str_replace(r$stim_filename,"_TR[:digit:]+ms","")
        }

        warn = paste0("Overriding File Name: Old (", r$stim_filename, ") to New (", newname, ") based on trigger_sensitivity.")
        warning(paste0(warn, "\n"))
        warnings_list <<- append(warnings_list, warn)
        r$stim_filename = newname
      }
      if (length(r$response_window) > 1) stop("ABORT: Multiple response windows:", r$response_window,". Aborting.")


      return(r)
    }


    Get_trial_data <- function() {
      Get_Delay_DF <- function(trial_data) {
        num_trials = nrow(trial_data)
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
        total_trials = trial_data %>% dplyr::count() %>% as.numeric()
        hits_calc = trial_data %>% dplyr::filter(Response == "Hit") %>% dplyr::count() %>% as.numeric()
        misses_calc = trial_data %>% dplyr::filter(Response == "Miss") %>% dplyr::count() %>% as.numeric()
        CRs_calc = trial_data %>% dplyr::filter(Response == "CR") %>% dplyr::count() %>% as.numeric()
        FAs_calc = trial_data %>% dplyr::filter(Response == "FA") %>% dplyr::count() %>% as.numeric()
        trial_count_go = trial_data %>% dplyr::filter(Trial_type != 0) %>% dplyr::count() %>% as.numeric()
        trial_count_nogo = trial_data %>% dplyr::filter(Trial_type == 0) %>% dplyr::count() %>% as.numeric()
        hit_percent = hits_calc / trial_count_go * 100

        FA_from_all_trials = trial_data %>% dplyr::filter(Trial_type == 5) %>% dplyr::count() %>% as.numeric() > 0 # catch oddball style where go trials can FA
        if (FA_from_all_trials) FA_percent = FAs_calc / total_trials * 100
        else if (trial_count_nogo > 0) FA_percent = FAs_calc / trial_count_nogo * 100
        else FA_percent = NA

        # Added to compare to written record at request of Undergrads
        writeLines(paste0("Trials: ", total_trials, "\tHit%: ", round(hit_percent, digits = 1), "\tFA%: ", round(FA_percent, digits = 1)))
        if (use_shiny) shiny::showNotification(glue("Trials: {total_trials}"))
        if (use_shiny) shiny::showNotification(glue("Hit%: {round(hit_percent, digits = 1)}"))
        if (use_shiny) shiny::showNotification(glue("FA%: {round(FA_percent, digits = 1)}"))

        if (results_total_trials == 0 | run_properties$stim_type == "train") {
          cat("Validate_Mat_Summary: Skipped (no summary)", sep = "\t", fill = TRUE)
        }
        else {
          # Check calculated stats against MATLAB summary stats
          if (total_trials != results_total_trials) stop(paste0("ABORT: Trial count summary (", results_total_trials, ") does not match Data (", total_trials, ")."))
          if (hits_calc != results_hits) stop(paste0("ABORT: Hit count summary (", results_hits, ") does not match Data (", hits_calc, ")."))
          if (misses_calc != results_misses) stop(paste0("ABORT: Miss count summary (", results_misses, ") does not match Data (", misses_calc, ")."))
          if (CRs_calc != results_CR) stop(paste0("ABORT: Correct rejection count summary (", results_CR, ") does not match Data (", CRs_calc, ")."))
          if (FAs_calc != results_FA) stop(paste0("ABORT: False alarm count summary (", results_FA, ") does not match Data (", FAs_calc, ")."))
        }
      }


      trial_data_encoded = data.frame(current_mat_file$result)

      # The MATLAB file has 2 extra columns for some unknown reason
      if (all(trial_data_encoded[7:8] != "0")) {
        stop("ABORT: Two extra columns. What are these columns storing?")
      } else {
        trial_data_encoded = trial_data_encoded[1:6]
      }

      names(trial_data_encoded) = list("Time_since_file_start_(s)", "Stim_ID", "Trial_type", "Attempts_to_complete", "Response", "Reaction_(s)")

      trial_data_encoded = trial_data_encoded %>%
        dplyr::mutate(Response = dplyr::case_when(Response == 1 ~ "Hit",
                                                  Response == 2 ~ "Miss",
                                                  Response == 3 ~ "FA",
                                                  Response == 4 ~ "CR",
                                                  TRUE ~ "ERROR"))

      trial_data = dplyr::left_join(x = trial_data_encoded,
                                  y = dplyr::select(run_properties$stim_encoding_table, -Repeat_number, -`Delay (s)`),
                                  by = "Stim_ID")
      trial_data = dplyr::bind_cols(trial_data, Get_Delay_DF(trial_data))

      #TODO: detect same day same rat data, renumber blocks in this list AND in master dataframe to continuous chronological order
      #see also: CheckMultipartRun
      block_list = rep(1:ceiling(nrow(trial_data)/run_properties$stim_block_size), each = run_properties$stim_block_size)

      trial_data = trial_data %>% dplyr::mutate(Trial_number = row_number(),
                                            Block_number = head(block_list, n = nrow(trial_data)))

      Validate_Mat_Summary()
      return(trial_data)
    }

    # These are typically demos or could trials that are ruled invalid by an observer.
    Omit_Trials <- function() {
      Query_Omitted_Trials <- function() {
        suppressMessages({
          omit_list = exclude_trials %>%
            stringr::str_replace_all(string = ., pattern = "[:space:]", replacement = "") %>%
            stringr::str_split(string = ., pattern = ",") %>%
            as_tibble(.name_repair = c("universal")) %>%
            dplyr::filter(...1 != "")
        })

       whole_numbers = omit_list %>%
          dplyr::filter(!str_detect(string = ...1, pattern = "-")) %>%
          as.list() %>% unlist(recursive = TRUE) %>% as.numeric()

       ranges = omit_list %>%
         dplyr::filter(str_detect(string = ...1, pattern = "-"))

       if(nrow(ranges)) {
         ranges = ranges %>%
           rowwise() %>%
           mutate(min = stringr::str_extract(string = ...1, pattern = "[:digit:]+(?=-)") %>% as.numeric(),
                  max = stringr::str_extract(string = ...1, pattern = "(?<=-)[:digit:]+") %>% as.numeric(),
                  s = list(seq(min,max))) %>%
           .$s %>% as.list() %>% unlist(recursive = TRUE)
         omit_list = c(whole_numbers, ranges)
       } else {
         omit_list = whole_numbers
       }

        omit_list = sort(omit_list)
        old_omit_list = unlist(tail(run_archive$omit_list, n = 1), recursive = TRUE)

        if (identical(omit_list, old_omit_list)) stop(paste0("ERROR: Stale exclude/omit list detected.\nPrior ",
                                                      tail(run_archive$rat_name, n = 1), " omitted trials: ",
                                                      paste(old_omit_list, collapse = " "), "\n",
                                                      "Current file ", run_properties$rat_name, " omitting: ",
                                                      paste(omit_list, collapse = " "), "\n",
                                                      "Please correct omit list and try again."))

        return(omit_list)
      }

      Remove_Trials <- function(omit_list) {
        # filter lines out of data
        omitted = trial_data %>% dplyr::filter(row_number() %in% omit_list)
        # TODO shove omitted to nonrat permanent rdata

        r = trial_data %>% dplyr::filter(!row_number() %in% omit_list)
        # get number of trials omitted
        omit_count = length(omit_list) %>% as.numeric()
        # calculate expected trials
        Trials_expected = trial_data %>% dplyr::count() %>% as.numeric() - omit_count
        # Calculate # of kept trials
        Trials = dplyr::count(r) %>% as.numeric()
        # check
        if (Trials != Trials_expected) stop("ABORT: Expected kept count does not match.")

        return(r)
      }


      omit_list = ""
      r = trial_data
      if (exclude_trials != "") omit_list = Query_Omitted_Trials()
      run_properties$omit_list <<- list(omit_list)
      if (rlang::is_empty(omit_list)) r = Remove_Trials(omit_list)
      return(r)
    }

    Number_Complete_Blocks <- function() {
      r = trial_data
      block_status = r %>%
        dplyr::group_by(Block_number) %>%
        dplyr::summarise(Block_number = unique(Block_number), size = n(), complete = size == run_properties$stim_block_size) %>%
        dplyr::filter(complete) %>%
        dplyr::mutate(complete_block_number = row_number()) %>%
        dplyr::select(Block_number, complete_block_number)

      r = dplyr::left_join(r, block_status, by = "Block_number")
      return(r)
    }



  # Import Workflow ---------------------------------------------------------
    cat("Loading file...", file_to_load, sep = "\t", fill = TRUE)
    current_mat_file = R.matlab::readMat(file_to_load)
    trial_collection = current_mat_file$stim[,,1] # all (1000) trial configurations, and the parameters specified to generate individual trials
    stim_encoding_table = Get_Stim_Encoding_Table() # the complete combinatoric list of trial configurations
    run_properties <- Get_Run_Properties()

    # l data from .mat file
    trial_data <- Get_trial_data()
    trial_data <- Omit_Trials()
    trial_data <- Number_Complete_Blocks()
    return(list("run_properties" = run_properties, "trial_data" = trial_data))
  }

  Identify_Analysis_Type <- function() {
    Get_File_Summary_BBN_Tone <- function() {
      # Make summary data table with step size
      run_properties$summary = file_frequency_ranges %>%
        dplyr::group_by(`Freq (kHz)`, `Delay (s)`, `Type`) %>%
        dplyr::summarise(dB_min = min(dB),
                         dB_max = max(dB),
                         dB_step_size = dB - lag(dB, default = first(dB)),
                         duration = list(run_properties$duration),
                         .groups = 'keep')
      # still grouped following this step, which is needed to remove the 1st row of each table that has a 0 step_size that is wrong for files with actual step_sizes

      if (!identical(run_properties$summary$dB_min, run_properties$summary$dB_max)) {
        run_properties$summary = run_properties$summary %>%
          dplyr::slice(-1) %>% # Drop 1st row of each sub-table
          .[!duplicated(.), ] # reduce to unique rows. Should be 1 row per frequency unless something is screwed up
      }

      # Check for mismatched step size
      if (length(unique(run_properties$summary$dB_step_size)) != 1) {
        warn = paste0("ACTION REQUIRED: Mismatched step size (", unique(run_properties$summary$dB_step_size), ").")
        warning(paste0(warn, "\n"))
        warnings_list <<- append(warnings_list, warn)
      }
      return(run_properties$summary)
    }

    Get_File_Summary_Oddball <- function() {
      # Make summary data table with step size
      go_freq = run_properties$stim_encoding_table %>%
        filter(Type == 1) %>%
        .$`Freq (kHz)`

      go_number = run_properties$stim_encoding_table %>%
        filter(Type == 1) %>%
        .$Stim_ID %>% as.character()

      spacer_number = run_properties$stim_encoding_table %>%
        filter(Type == 0 & `Dur (ms)` >= user_settings$oddball_wait_min  &  `Dur (ms)` < user_settings$oddball_wait_max) %>%
        .$Stim_ID %>% as.character()

      nogo_freq = run_properties$stim_encoding_table %>%
        filter(Type == 0 & `Dur (ms)` < user_settings$oddball_wait_min) %>%
        .$`Freq (kHz)`

      go_dB = run_properties$stim_encoding_table %>%
        filter(Type == 1) %>%
        .$`Inten (dB)`

      nogo_dB = run_properties$stim_encoding_table %>%
        filter(Type == 0 & `Dur (ms)` < user_settings$oddball_wait_min) %>%
        .$`Inten (dB)`

      go_position_range = run_properties$stim_encoding_table %>%
        filter(`Train Setting` != "" & Repeat_number != 0 & Type != 0) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(Temp_Position = stringr::str_remove_all(`Train Setting`, paste0("[", spacer_number, " ]")), # strip characters from train so we can determine go tone position directly
                      Position = stringr::str_locate(pattern = go_number, Temp_Position)) %>% # positions of go tones
        .$Position %>% as.data.frame() %>% .[,1] # str_locate gives start and stop positions, we just need either one

      catch = run_properties$stim_encoding_table %>%
        filter(Type == 0 & Repeat_number > 0) %>%
        nrow() > 0

      odds = run_properties$stim_encoding_table %>%
        filter(Type == 5) %>%
        .$Repeat_number %>% unique() %>%
        tibble(odds = ., position = go_position_range)

      run_properties$summary = c(
        go_freq = go_freq,
        go_dB = go_dB,
        nogo_freq = nogo_freq,
        nogo_dB  = nogo_dB,
        go_position_start = min(go_position_range),
        go_position_stop = max(go_position_range),
        catch = catch,
        odds = odds
      )

      return(run_properties$summary)
    }

    ID_Tonal <- function() {
      r = NULL
      # Determine if it has custom ranges (i.e. not all frequencies have the same range)
      has_different_dB_ranges_for_frequencies = length(unique(run_properties$summary$dB_min)) != 1 | length(unique(run_properties$summary$dB_max)) != 1
      # Determine if octave file (in that case one of the normal intensity (dB) should be 0 or non-rewarded)
      # Note that for each type 1 and type 0 the min & max should be equal (i.e. one intensity) but not necessarily between type 1 & 0
      has_audible_NoGo = any(run_properties$summary$Type == 0)
      # Test octave files have multiple types of No Go trials in the audible range
      has_more_than_one_NoGo = length(unique(dplyr::filter(run_properties$summary, Type == 0)$`Freq (kHz)`)) > 1
      # Does the file have only a single frequency
      has_only_one_frequency = length(unique(run_properties$stim_encoding_table$`Freq (kHz)`)) == 1
      # Does the file have only a single intensity (dB), i.e. training
      has_one_dB = unique(run_properties$summary$dB_min == run_properties$summary$dB_max)
      # 50ms is used for Oddball training while 300ms is used for Octave training
      short_duration = run_properties$summary %>% unnest(duration) %>% .$`Dur (ms)` %>% unique == 50

      # For tonal files (octaves, or mainly 4-32kHz)
      # DO NOT CHANGE THE TEXTUAL DESCRIPTIONS OR YOU WILL BREAK COMPARISONS LATER
      if (!has_audible_NoGo & has_different_dB_ranges_for_frequencies) r = "Tone (Thresholding)"
      else if (has_one_dB & has_only_one_frequency) r = "Training - Tone"
      else if (has_audible_NoGo & has_more_than_one_NoGo) r = "Octave"
      else if (has_audible_NoGo & !has_more_than_one_NoGo & short_duration) r = "Training - Oddball"
      else if (has_audible_NoGo & !has_more_than_one_NoGo & !short_duration) r = "Training - Octave"
      else if (!has_audible_NoGo & has_only_one_frequency ) r = "Tone (Single)"
      else if (!has_audible_NoGo) r =  "Tone (Standard)"
      else (stop("ABORT: Unknown tonal file type."))

      return(r)
    }

    ID_BBN <- function() {
      r = NULL
      has_one_dB = run_properties$summary$dB_min == run_properties$summary$dB_max
      has_multiple_durations = length(unique(run_properties$stim_encoding_table$`Dur (ms)`)) > 1

      # For broadband files (training or otherwise)
      # DO NOT CHANGE THE TEXTUAL DESCRIPTIONS OR YOU WILL BREAK COMPARISONS LATER
      if (has_one_dB) r = "Training - BBN"
      else if (has_multiple_durations) r = "BBN Mixed Duration"
      else r = "BBN (Standard)"
      return(r)
    }

    ID_Gap <- function() {
      r = NULL
      has_one_dB = unique(run_properties$summary$dB_min) == unique(run_properties$summary$dB_max)
      has_multiple_durations = length(unique(filter(run_properties$stim_encoding_table, Type == 1)$`Dur (ms)`)) > 1

      # For gap detection files (training or otherwise)
      # DO NOT CHANGE THE TEXTUAL DESCRIPTIONS OR YOU WILL BREAK COMPARISONS LATER
      if (has_one_dB & !has_multiple_durations) r = "Training - Gap"
      else if (has_one_dB & has_multiple_durations) r = "Gap (Standard)"
      else (stop("ABORT: Unknown tonal file type."))
      return(r)
    }

    ID_Oddball <- function() {
      r = NULL
      # Determine if catch trials
      has_catch_trials = run_properties$summary$catch
      # Determine if even odds
      has_uneven_trial_odds = length(unique(run_properties$summary$odds.odds)) > 1
      has_BG = run_properties$background_file != "None"

      # For Oddball files (training or otherwise)
      # DO NOT CHANGE THE TEXTUAL DESCRIPTIONS OR YOU WILL BREAK COMPARISONS LATER
      if (has_catch_trials & has_uneven_trial_odds) r = "Oddball (Uneven Odds & Catch)"
      else if (has_uneven_trial_odds) r = "Oddball (Uneven Odds)"
      else if (has_catch_trials) r = "Oddball (Catch)"
      else if (has_BG) r = "Oddball (Background)"
      else if (!has_catch_trials & !has_uneven_trial_odds) r = "Oddball (Standard)"
      else stop("ABORT: Unknown Oddball file type.")
      return(r)
    }


    # Identify Analysis Workflow ----------------------------------------------
    # Get ranges for each frequency
    file_frequency_ranges = run_properties$stim_encoding_table %>%
      dplyr::filter(`Inten (dB)` != -100) %>% # Remove No-Go from range
      dplyr::group_by(`Freq (kHz)`, `Delay (s)`, `Type`, `Repeat_number`) %>%
      dplyr::summarise(dB = unique(`Inten (dB)`), .groups = 'drop') # Get each unique dB

    if (run_properties$stim_type == "BBN" | run_properties$stim_type == "tone" | run_properties$stim_type == "gap") run_properties <<- append(run_properties, list(summary = Get_File_Summary_BBN_Tone()))
    else if (run_properties$stim_type == "train") run_properties <<- append(run_properties, list(summary = Get_File_Summary_Oddball()))
    else stop(paste0("ABORT: Unknown stim type: ", run_properties$stim_type))

    r = NULL
    if (run_properties$stim_type == "tone") r = ID_Tonal()
    if (run_properties$stim_type == "BBN") r = ID_BBN()
    if (run_properties$stim_type == "gap") r = ID_Gap()
    if (run_properties$stim_type == "train") r = ID_Oddball()

    if (is.null("r")) stop("ABORT: Unknown file type. Can not proceed with analysis")

    r = list(type = r,
             minimum_trials = user_settings$minimum_trials[[r]])

    return(r)
  }

  Build_Filename <- function() {
    Check_Delay <- function(expected_delay) {
      if (run_properties$delay != expected_delay) {
        warn = paste0("ACTION REQUIRED: wrong delay window (", run_properties$delay, ") does not match expectation (", expected_delay, ")")
        warning(paste0(warn, "\n"))
        warnings_list <<- append(warnings_list, warn)
      }
    }

    Tonal_Filename <- function() {
      if (analysis$type == "Tone (Standard)") {
        go_kHz_range = paste0(run_properties$summary %>% dplyr::filter(Type == 1) %>% .$`Freq (kHz)` %>% min(), "-",
                              run_properties$summary %>% dplyr::filter(Type == 1) %>% .$`Freq (kHz)` %>% max(), "kHz")
        go_dB_range = paste0(run_properties$summary %>% dplyr::filter(Type == 1) %>% .$dB_min %>% unique(), "-",
                             run_properties$summary %>% dplyr::filter(Type == 1) %>% .$dB_max %>% unique(), "dB")

        computed_file_name = paste0(go_kHz_range, "_", go_dB_range, "_", run_properties$duration, "ms_", run_properties$lockout, "s")
      }

      else if (analysis$type == "Tone (Single)") {
        go_kHz = paste0(run_properties$summary %>% dplyr::filter(Type == 1) %>% .$`Freq (kHz)`, "kHz")
        go_dB_range = paste0(run_properties$summary %>% dplyr::filter(Type == 1) %>% .$dB_min %>% unique(), "-",
                             run_properties$summary %>% dplyr::filter(Type == 1) %>% .$dB_max %>% unique(), "dB")

        computed_file_name = paste0(go_kHz, "_", go_dB_range, "_", run_properties$duration, "ms_", run_properties$lockout, "s")
      }

      else if (analysis$type == "Tone (Thresholding)") {
        go_kHz_range = paste0(run_properties$summary %>% dplyr::filter(Type == 1) %>% .$`Freq (kHz)` %>% min(), "-",
                              run_properties$summary %>% dplyr::filter(Type == 1) %>% .$`Freq (kHz)` %>% max(), "kHz")
        dB_step_size = unique(run_properties$summary$dB_step_size) %>% as.numeric()
        if (dB_step_size == 5) {
          go_dB_range = "MIX5stepdB"
          analysis$prepend_name <<- TRUE
        }
        else if (dB_step_size == 10) {
          go_dB_range = "MIXdB"
          analysis$prepend_name <<- TRUE
        }
        else stop("ABORT: Tone (Thresholding): Unrecognized dB_step_size (", dB_step_size,"). Aborting.")
        rat_ID = stringr::str_split(run_properties$stim_filename, "_") %>% unlist() %>% .[1]
        computed_file_name = paste0(rat_ID, "_", go_kHz_range, "_", go_dB_range, "_", run_properties$duration, "ms_", run_properties$lockout, "s")
      }
      else if (analysis$type == "Training - Oddball")
      {
        go_kHz = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 1) %>% .$`Freq (kHz)`, "kHz_")
        nogo = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 0) %>% .$`Freq (kHz)`)
        nogo_kHz = if_else(nogo == 0, "BBN_", paste0(nogo, "kHz_"))
        go_dB = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 1) %>% .$`Inten (dB)`, "dB_")
        nogo_dB = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 0) %>% .$`Inten (dB)`, "dB_")
        catch_number = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 0) %>% .$Repeat_number)

        computed_file_name = paste0(go_kHz, go_dB, nogo_kHz, nogo_dB, run_properties$duration, "ms_", run_properties$lockout, "s")
        if (catch_number != 3) computed_file_name = paste0(computed_file_name, "_c", catch_number)

        analysis$minimum_trials <<- user_settings$minimum_trials$`Training - Oddball`
      }

      else if (analysis$type == "Training - Tone") {
        go_kHz = paste0(run_properties$summary %>% dplyr::filter(Type == 1) %>% .$`Freq (kHz)`, "kHz_")
        go_dB = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 1) %>% .$`Inten (dB)`, "dB_")
        catch_number = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 0) %>% .$Repeat_number) %>% as.numeric()
        delay = run_properties$delay %>% stringr::str_replace(" ", "-")
        lockout = `if`(length(run_properties$lockout) > 0, run_properties$lockout, 0)

        rat_ID = filter(rat_archive, Rat_name == run_properties$rat_name & is.na(end_date))$Rat_ID

        computed_file_name = paste0(go_kHz, go_dB) %>% print
        if (length(catch_number) == 0) {
          computed_file_name = paste0(computed_file_name, delay, "s_0catch")
          delay_in_filename <<- TRUE
        }
        else if (catch_number > 0) {
          if(rat_archive[rat_archive$Rat_ID == rat_ID,]$Assigned_Detail == "Oddball" |
             rat_archive[rat_archive$Rat_ID == rat_ID,]$Assigned_Phase == "Octave" ) {
            computed_file_name = paste0(computed_file_name, run_properties$duration, "ms_", lockout, "s")
            delay_in_filename <<- FALSE
            analysis$minimum_trials <<- user_settings$minimum_trials$`Tone (Single)`

          } else {
            computed_file_name = paste0(computed_file_name, catch_number, "catch_", lockout, "s")
            delay_in_filename <<- FALSE

            if (catch_number >= 3) {
              analysis$minimum_trials <<- user_settings$minimum_trials$`Tone (Single)`
            }
          }
        }
      }

      else if (analysis$type == "Octave")
      {
        go_kHz = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 1) %>% .$`Freq (kHz)`, "-")
        nogo_kHz = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 0) %>% dplyr::arrange(Stim_ID) %>% tail(n = 1) %>% .$`Freq (kHz)`, "kHz_")
        nogo_kHz2 = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 0) %>% dplyr::arrange(Stim_ID) %>% tail(n = 2) %>% head(n = 1) %>% .$`Freq (kHz)` %>% round(digits = 1), "kHz_")
        go_dB = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 1) %>% .$`Inten (dB)`)
        nogo_dB = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 0) %>% dplyr::arrange(Stim_ID) %>% tail(n = 1) %>% .$`Inten (dB)`)
        has_dB_range = go_dB != nogo_dB

        computed_file_name1 = paste0(go_kHz, nogo_kHz)
        computed_file_name2 = paste0(go_kHz, nogo_kHz2)
        if (has_dB_range) {
          computed_file_name1 = paste0(computed_file_name1, go_dB, "-")
          computed_file_name2 = paste0(computed_file_name2, go_dB, "-")
        }
        computed_file_name1 = paste0(computed_file_name1, nogo_dB, "dB_", run_properties$duration, "ms_", run_properties$lockout, "s")
        computed_file_name2 = paste0(computed_file_name2, nogo_dB, "dB_", run_properties$duration, "ms_", run_properties$lockout, "s")
        # computed_file_name = list(computed_file_name, computed_file_name2) # list should be safe from the later pastes because they shouldn't fire


        # Test for 1/6 or the zoom in on 1/12. If it is 1/12 then there will be odd steps
        stim_in_octave = run_properties$stim_encoding_table %>% mutate(octave_fraction = log(as.numeric(str_extract(go_kHz, pattern = "[:digit:]+"))/`Freq (kHz)`)/log(2),
                                                                       octave_step = abs(round(octave_fraction * 12)),
                                                                       even = (octave_step %% 2) == 0)
        is_6th_of_octave = all(stim_in_octave$even == TRUE)

        if(is_6th_of_octave) computed_file_name = computed_file_name1
        else computed_file_name = computed_file_name2

        return(computed_file_name)
      }

      else if (analysis$type == "Training - Octave")
      {
        go_kHz = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 1) %>% .$`Freq (kHz)`, "kHz_")
        nogo_kHz = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 0) %>% .$`Freq (kHz)`, "kHz_")
        go_dB = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 1) %>% .$`Inten (dB)`, "dB_")
        nogo_dB = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 0) %>% .$`Inten (dB)`, "dB_")
        catch_number = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 0) %>% .$Repeat_number)

        computed_file_name = paste0(go_kHz, go_dB, nogo_kHz, nogo_dB, run_properties$duration, "ms_", run_properties$lockout, "s")
        if (catch_number != 3) computed_file_name = paste0(computed_file_name, "_c", catch_number)
      }

      response_window = unique(run_properties$stim_encoding_table["Nose Out TL (s)"]) %>% as.numeric()
      has_Response_window = response_window != 2
      has_TR = run_properties$trigger_sensitivity != 200
      has_BG = run_properties$background_type != "None"
      BG = if (has_BG) paste0(stringr::str_remove(pattern = ".mat", string = run_properties$background_file), "_", run_properties$background_dB, "dB")

      if (has_Response_window) computed_file_name = paste0(computed_file_name, "_", response_window, "s")
      if (has_TR & analysis$type != "Tone (Thresholding)") computed_file_name = paste0(computed_file_name, "_", "TR", run_properties$trigger_sensitivity, "ms")
      if (has_BG) computed_file_name = paste0(computed_file_name, "_", BG)

      return(computed_file_name)
    }

    BBN_Filename <- function() {
      if (analysis$type == "Training - BBN") {
        go_dB = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 1) %>% .$`Inten (dB)`, "dB_")
        catch_number = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 0) %>% .$Repeat_number) %>% as.numeric()
        delay = run_properties$delay %>% stringr::str_replace(" ", "-")
        lockout = `if`(length(run_properties$lockout) > 0, run_properties$lockout, 0)

        computed_file_name = paste0("BBN_", go_dB)
        if (length(catch_number) == 0) {
          computed_file_name = paste0(computed_file_name, delay, "s_0catch")
          delay_in_filename <<- TRUE
        }
        else if (catch_number > 0) {
          computed_file_name = paste0(computed_file_name, catch_number, "catch_", lockout, "s")
          delay_in_filename <<- FALSE

          if (catch_number >= 3) {
            analysis$minimum_trials <<- user_settings$minimum_trials$`BBN (Standard)`
          }
        }
      }
      else
      {
        go_dB_range = paste0(run_properties$summary %>% dplyr::filter(Type == 1) %>% .$dB_min %>% unique(), "-",
                             run_properties$summary %>% dplyr::filter(Type == 1) %>% .$dB_max %>% unique(), "dB")

        has_duration_range = nrow(unique(run_properties$duration)) > 1
        if (has_duration_range) {
          duration = paste0(min(run_properties$duration), "-",
                            max(run_properties$duration), "")
        } else {
          duration = run_properties$duration
        }

        response_window = unique(run_properties$stim_encoding_table["Nose Out TL (s)"]) %>% as.numeric()
        has_Response_window = response_window != 2
        has_TR = run_properties$trigger_sensitivity != 200
        has_BG = run_properties$background_type != "None"

        BG = if (has_BG) paste0(stringr::str_remove(pattern = ".mat", string = run_properties$background_file), "_", run_properties$background_dB, "dB")

        computed_file_name = paste0("BBN_", go_dB_range, "_", duration, "ms_", run_properties$lockout, "s")
        if (has_Response_window) computed_file_name = paste0(computed_file_name, "_", response_window, "s")
        if (has_TR) computed_file_name = paste0(computed_file_name, "_", "TR", run_properties$trigger_sensitivity, "ms")
        if (has_BG) computed_file_name = paste0(computed_file_name, "_", BG)
      }
      return(computed_file_name)
    }

    Gap_Filename <- function() {
      if (analysis$type == "Training - Gap") {
        go_dB = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 1) %>% .$`Inten (dB)`, "dB_")
        catch_number = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 0) %>% .$Repeat_number) %>% as.numeric()
        duration = unique(filter(run_properties$stim_encoding_table, Type == 1)$`Dur (ms)`) %>% as.numeric()
        delay = run_properties$delay %>% stringr::str_replace(" ", "-")
        lockout = `if`(length(run_properties$lockout) > 0, run_properties$lockout, 0)

        computed_file_name = paste0("gap_", go_dB)
        if (length(catch_number) == 0) {
          computed_file_name = paste0(computed_file_name, duration, "ms_", delay, "s_0catch")
          delay_in_filename <<- TRUE
        }
        else if (catch_number > 0) {
          computed_file_name = paste0(computed_file_name, duration, "ms_", catch_number, "catch_", lockout, "s")
          delay_in_filename <<- FALSE

          if (catch_number >= 3) {
            analysis$minimum_trials <<- user_settings$minimum_trials$`Gap (Standard)`
          }
        }
      }
      else
      {
        go_dB = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 1) %>% .$`Inten (dB)` %>% unique(), "dB")
        catch_number = paste0(run_properties$stim_encoding_table %>% dplyr::filter(Type == 0) %>% .$Repeat_number) %>% as.numeric()

        has_duration_range = filter(run_properties$stim_encoding_table, Type != 0) %>% unique() %>% nrow() > 1
        if (has_duration_range) {
          duration = run_properties$stim_encoding_table %>%
            filter(Type != 0) %>%
            transmute(x = paste0(min(`Dur (ms)`), "-",
                                 max(`Dur (ms)`), "")) %>% .$x %>% unique()
        } else {
          duration = filter(run_properties$stim_encoding_table, Type != 0) %>% .$`Dur (ms)`
        }

        response_window = unique(run_properties$stim_encoding_table["Nose Out TL (s)"]) %>% as.numeric()
        has_Response_window = response_window != 2
        has_TR = run_properties$trigger_sensitivity != 200

        # Note there is not BG test here because there is expected to always be background in a Gap Detection file.

        computed_file_name = paste0("gap_", go_dB, "_", duration, "ms_")
        if (catch_number != 3) computed_file_name = paste0(computed_file_name, catch_number, "catch_")
        computed_file_name = paste0(computed_file_name, run_properties$lockout, "s")
        if (has_Response_window) computed_file_name = paste0(computed_file_name, "_", response_window, "s")
        if (has_TR) computed_file_name = paste0(computed_file_name, "_", "TR", run_properties$trigger_sensitivity, "ms")
      }
      return(computed_file_name)
    }

    Oddball_Filename <- function() {
      expected_delay <<- user_settings$delay_oddball
      if (run_properties$summary$nogo_freq == 0) nogo_freq = "BBN"
      else nogo_freq = paste0(run_properties$summary$nogo_freq, "kHz")

      computed_file_name = paste0(run_properties$summary$go_freq, "kHz_", run_properties$summary$go_dB, "dB_", nogo_freq, "_",
                                  run_properties$summary$nogo_dB, "dB_", run_properties$lockout, "s_", run_properties$summary$go_position_start, "-", run_properties$summary$go_position_stop)
      if (analysis$type == "Oddball (Uneven Odds & Catch)") computed_file_name = paste0(computed_file_name, "_odds_NG")
      if (analysis$type == "Oddball (Uneven Odds)") computed_file_name = paste0(computed_file_name, "_odds")
      if (analysis$type == "Oddball (Catch)") computed_file_name = paste0(computed_file_name, "_catch")
      if (analysis$type == "Oddball (Background)") computed_file_name = paste0(computed_file_name, "_",
                                                                               str_extract(run_properties$background_file, pattern = "^.*(?=.mat)"),
                                                                               "_", run_properties$background_dB, "dB")

      return(computed_file_name)
    }



  # Build Filename Workflow -------------------------------------------------
    delay_in_filename = FALSE
    expected_delay = user_settings$delay_default
    computed_file_name = switch(run_properties$stim_type,
                                "tone" = Tonal_Filename(),
                                "BBN" = BBN_Filename(),
                                "gap" = Gap_Filename(),
                                "train" = Oddball_Filename())

    has_different_NG = run_properties$nogo_max_touching != 1
    if (has_different_NG) computed_file_name = paste0(computed_file_name, "_NG", run_properties$nogo_max_touching)

    if (is.null(computed_file_name)) stop("ABORT: Unknown file type. Can not create filename.")

    if (!delay_in_filename) Check_Delay(expected_delay)

    if (length(computed_file_name) > 1 ) {
      if (!(run_properties$stim_filename %in% computed_file_name)) {
        warn = paste0("\nCaution: mismatch in provided filename: ", run_properties$stim_filename, "\n",
                      "       potentially acceptable filename: ", computed_file_name, "")
        warning(paste0(warn, "\n"))
        warnings_list <<- append(warnings_list, warn)
        computed_file_name = "BAD_FILENAME" #TODO: User input to choose which of the computed filenames to use
      }
      else {
        computed_file_name = run_properties$stim_filename #at this point we've guaranteed file_name and (one of the) computed_file_name match, so we can just return file_name, i.e. the match
      }
    } else {
      if (run_properties$stim_filename != computed_file_name) {
        warn = paste0("Caution: mismatch in provided filename: ", run_properties$stim_filename, "\n  ",
                      "       calculated filename by contents: ", computed_file_name)
        warning(paste0(warn, "\n"))
        warnings_list <<- append(warnings_list, warn)
      }
    }

    # we return COMPUTED filename because the stored one could have errors like 'db' for 'dB' etc
    return(computed_file_name)
  }

  Check_Assigned_Filename <- function() {
    r = TRUE
    date = run_properties$creation_time %>% stringr::str_sub(1,8) %>% as.numeric()

    if(old_file) {
      # need to fetch from old_excel_archive
      date_asDate <<- paste0(stringr::str_sub(date, 1, 4), "-", stringr::str_sub(date, 5, 6), "-", stringr::str_sub(date, 7, 8)) %>% as.Date()
      old_data <<- old_excel_archive %>% dplyr::filter(Date == date_asDate & rat_name == run_properties$rat_name)
      analysis$assigned_file_name <<- old_data$Filename
      if(rlang::is_empty(analysis$assigned_file_name)) {
        warn = paste0("No assigned file name found in excel document for ", run_properties$rat_name, " on ", date, ".")
        warnings_list <<- append(warnings_list, warn)
      }
    } else {
      id = Get_Rat_ID(run_properties$rat_name)
      if(length(id) == 0) stop("ABORT: Unknown rat ID.")
      rat_data = rat_archive[rat_archive$Rat_ID == id,]
      analysis$assigned_file_name <<- rat_data$Assigned_Filename
      if(rlang::is_na(analysis$assigned_file_name)) {
        warn = paste0("No assigned file name found in rat_archive for ", run_properties$rat_name, " on ", date, ".")
        analysis$assigned_file_name <<- "none"
        warnings_list <<- append(warnings_list, warn)
      }
    }

    # handle customized individual files
    if(analysis$prepend_name) {
      analysis$assigned_file_name = paste0(run_properties$rat_name, "_", analysis$assigned_file_name)
    }

    if (analysis$computed_file_name != analysis$assigned_file_name ) {
      warn = paste0("ACTION REQUIRED: Was rat run on the wrong file?\n",
                    "ERROR: Filename -- ", analysis$computed_file_name, " -- does not match\n",
                    "     Assignment -- ", analysis$assigned_file_name, "")
      warning(paste0(warn, "\n"))
      warnings_list <<- append(warnings_list, warn)
      r = FALSE
    }
    return(r)
  }

  Check_UUID <- function() {
    Build_UUID <- function() {

      trial1_start_time = trial_data$`Time_since_file_start_(s)`[1]
      trial1_delay_time = trial_data$`Delay (s)`[1]
      current_file_UUID = paste0(run_properties$creation_time, "_", trial1_start_time, "_", trial1_delay_time)

      return(current_file_UUID)
    }

    Is_New_UUID <- function(uuid) {
      is_old = uuid %in% run_archive$UUID
      if (is_old) {
        warn = paste0("SKIPPED: This file has already been added: ", uuid)
        warning(paste0(warn, "\n"))
        warnings_list <<- append(warnings_list, warn)
        #stop(paste0("ABORT: This file has already been added: ", uuid))
      }
      return(!is_old)
    }

    run_properties$UUID <<- Build_UUID()
    return(Is_New_UUID(run_properties$UUID))
  }

  Calculate_Summary_Statistics <- function() {
    # Creates a properly formatted table for psycho by adding the overall CR/FA to each row
    Format_for_Psycho <- function(df) {
      check = df %>% filter(Type == 0) %>% count() %>% as.numeric()
      CRnum = (if (check == 1) filter(df, Type == 0) %>% .$CR %>% as.numeric() else check)
      suppressWarnings({
        FAnum = (if (check == 1) filter(df, Type == 0) %>% .$FA %>% as.numeric() else check)
      })
      if(!("Hit" %in% colnames(df))) df = df %>% add_column(Hit = NA)
      if(!("Miss" %in% colnames(df))) df = df %>% add_column(Miss = NA)
      if(!("FA" %in% colnames(df))) {
        df = df %>% add_column(FA = NA)
        FAnum = 0
      }
      if(!("CR" %in% colnames(df))) {
        df = df %>% add_column(CR = NA)
        CRnum = 0
      }

      new_df = df %>% filter(Type == 1) %>%
        mutate(CR = ifelse(is.na(CR), CRnum, CR),
               FA = ifelse(is.na(FA), FAnum, CR),
               Hit = as.numeric(Hit),
               Miss = as.numeric(Miss)) %>% replace(is.na(.), 0)
      return(new_df)
    }

    Format_for_Psycho_Octave <- function(df) {
      check = df %>% filter(Type == 1) %>% count() %>% as.numeric()
      Hitnum = (if (check == 1) filter(df, Type == 1) %>% .$Hit %>% as.numeric() else check)
      Missnum = (if (check == 1) filter(df, Type == 1) %>% .$Miss %>% as.numeric() else check)
      if(!("CR" %in% colnames(df))) df = df %>% add_column(CR = NA)
      if(!("FA" %in% colnames(df))) df = df %>% add_column(FA = NA)
      if(!("Hit" %in% colnames(df))) {
        df = df %>% add_column(Hit = NA)
        Hitnum = 0
      }
      if(!("Miss" %in% colnames(df))) {
        df = df %>% add_column(Miss = NA)
        Missnum = 0
      }

      new_df = df %>% filter(Type == 0) %>%
        mutate(CR = as.numeric(CR),
               FA = as.numeric(FA),
               Hit = ifelse(is.na(Hit), Hitnum, Hit),
               Miss = ifelse(is.na(Miss), Missnum, Miss)) %>% replace(is.na(.), 0)
      return(new_df)
    }

    # Signal detection index calculation
    Calculate_dprime <- function(df) {
      r = dprime(n_hit = df$Hit,
                 n_fa = df$FA,
                 n_miss = df$Miss,
                 n_cr = df$CR,
                 adjusted = TRUE)
      r[["bppd"]] = NULL # drop this column always (it can be wrongly dimensioned and break things)
      r = r %>% as_tibble() %>%
        mutate(dB = df$`Inten (dB)`,
               Freq = df$`Freq (kHz)`,
               Dur = df$`Dur (ms)`)
      return(r)
    }

    # Threshold calculation calculation based on TH_cutoff intercept of fit curve
    # LOESS: Local Regression is a non-parametric approach that fits multiple regressions
    # see http://r-statistics.co/Loess-Regression-With-R.html
    Calculate_TH <- function(df) {
      # Uncomment to see line fitting by a package which shows line
      # library(drda)
      # drda(dprime ~ dB, data = df) %>% plot
      fit = loess(dprime ~ dB, data = df)
      # plot(fit)
      TH = approx(x = fit$fitted, y = fit$x, xout = user_settings$TH_cutoff, ties = "ordered")$y
      # print(TH)
      return(TH)
    }

    Calculate_TH_gap <- function(df) {
      # Uncomment to see line fitting by a package which shows line
      # library(drda)
      # drda(dprime ~ dB, data = df) %>% plot
      fit = loess(dprime ~ Dur, data = df)
      # plot(fit)
      TH = approx(x = fit$fitted, y = fit$x, xout = user_settings$TH_cutoff, ties = "ordered")$y
      # print(TH)
      return(TH)
    }

    Calculate_Threshold <- function() {
      # Signal detection index calculation by the psycho package. We use d' a sensitivity measure.
      # https://neuropsychology.github.io/psycho.R/2018/03/29/SDT.html

      # Calculate d' and save (along with hit/miss/CR/FA table)
      dprime_table <-
        trial_data %>% #TODO add a way to calculate using full trials archive history
        dplyr::filter(Block_number != 1) %>%
        group_by(`Dur (ms)`, Type, `Freq (kHz)`, `Inten (dB)`, Response) %>%
        summarise(count = n(), .groups = "keep") %>%
        spread(Response, count) %>% #View
        ungroup()

      dprime_table = Format_for_Psycho(dprime_table)
      dprime_data = Calculate_dprime(dprime_table)
      dprime <<- select(dprime_data, Freq, dB, Dur, dprime)
      # save this to stats

      # Check for to small a dataset to calculate TH
      less_than_two_blocks = is.na(trial_data %>% #TODO add a way to calculate using full trials archive history
                                    dplyr::filter(Block_number > 1) %>% .$complete_block_number %>% unique() %>% {if(is_empty(.)) {NA} else {head(., n = 1)} })
      if(less_than_two_blocks){
        r = dprime_data %>%
          select(Freq, Dur, dB, dprime) %>%
          group_by(Freq, Dur) %>%
          nest() %>%
          mutate(TH = NA_integer_) %>%
          select(-data)

        # Warning
        warn = paste0("Can not caluclate TH due to < 1 block of trials.")
        warning(paste0(warn, "\n"))
        warnings_list <<- append(warnings_list, warn)

      } else {
        groupings = case_when(analysis$type == "Gap (Standard)" ~ c("Freq", "dB"),
                              TRUE ~ c("Freq", "Dur"))

        r = dprime_data %>%
          select(Freq, Dur, dB, dprime) %>%
          group_by_at(groupings)

        # To remove psycho warnings cause by a single dprime, check to see if there are multiple dprimes
        need_evaluation = r %>% summarise(dprime_check = unique(dprime) %>% as.list() %>% length,
                                          .groups = "keep") %>% filter(dprime_check > 1)

        need_evaluation = left_join(need_evaluation, r, by = groupings) %>% select(-dprime_check) %>% nest()

        if(nrow(need_evaluation) == 0) {
          r$TH = NA_integer_
        } else if (analysis$type == "Gap (Standard)") {
          need_evaluation = need_evaluation %>%
            mutate(TH = map_dbl(data, Calculate_TH_gap))
        } else {
          need_evaluation = need_evaluation %>%
            mutate(TH = map_dbl(data, Calculate_TH))
        }

        need_evaluation = select(need_evaluation, -data)

        r = left_join(r, need_evaluation, by = groupings) %>%
          select(-dB, -dprime) %>%
          unique()
      }

      return(r)
    }

    Calculate_Reaction_Time <- function(audible_only = FALSE, min_time_s = 0.015) {
      Filter_to_Audible <- function(df) { #TODO do we WANT filtered to audible? Probably no for graph, yes for analyses
        ms = unique(df$`Dur (ms)`)
        kHz = unique(df$`Freq (kHz)`)

        #TODO: use overall cutoff rather than daily
        cutoff = TH_by_frequency_and_duration %>% # have to use UQ to force the evaluation of the variable
          filter(Dur == UQ(ms) & Freq == UQ(kHz)) %>% .$TH

        r = df %>% filter(`Inten (dB)` >= UQ(cutoff))
        return(r)
      }

      r = trial_data %>% dplyr::filter(Response == "Hit")

      r = dplyr::filter(r, `Reaction_(s)` > min_time_s)

      if (audible_only) {
        r = r %>%
          mutate(Dur = `Dur (ms)`, Freq = `Freq (kHz)`) %>%
          group_by(Freq, Dur) %>%
          nest() %>%
          mutate(Rxn = map(.x = data, .f = Filter_to_Audible)) %>%
          select(-data) %>%
          unnest(Rxn)
      }

        r = r %>%
          group_by(`Dur (ms)`, `Freq (kHz)`, `Inten (dB)`) %>%
          summarise(Rxn = mean(`Reaction_(s)`, na.rm = T), .groups = "keep")
      return(r)
    }

    Calculate_FA_Detailed_Oddball <- function() {
      r = trial_data %>%
        filter(Trial_type != 0) %>% # rule out catch trials
        rename(position = `Inten (dB)`) %>%
        group_by(position) %>%
        summarise(FA = sum(Response == 'FA'),
                  trials = n(),
                  FA_percent_detailed = FA/trials)

      return(r)
    }

    Calculate_FA_Detailed_Octave <- function() {
      dprime_table <-
        trial_data %>%
        dplyr::filter(Block_number != 1) %>%
        group_by(`Dur (ms)`, Type, `Freq (kHz)`, `Inten (dB)`, Response) %>%
        summarise(count = n(), .groups = "keep") %>%
        spread(Response, count) %>% #View
        ungroup()

      dprime_table = Format_for_Psycho_Octave(dprime_table) # type = 0 for inverted (no-go) d'
      dprime_data = Calculate_dprime(dprime_table) %>%
        rename(`Freq (kHz)` = Freq)

      r = trial_data %>%
        filter(Trial_type == 0) %>% # select no-go trials
        group_by(`Freq (kHz)`) %>%
        summarise(FA = sum(Response == 'FA'),
                  trials = n(),
                  FA_percent_detailed = FA/trials) %>%
        left_join(dprime_data %>% select(`Freq (kHz)`, dprime), by = "Freq (kHz)")

      return(r)
    }



    # Statistics Workflow -----------------------------------------------------

    trial_count = trial_data %>% dplyr::count() %>% as.numeric()
    hits = trial_data %>% dplyr::filter(Response == "Hit") %>% dplyr::count() %>% as.numeric()
    misses = trial_data %>% dplyr::filter(Response == "Miss") %>% dplyr::count() %>% as.numeric()
    CRs = trial_data %>% dplyr::filter(Response == "CR") %>% dplyr::count() %>% as.numeric()
    FAs = trial_data %>% dplyr::filter(Response == "FA") %>% dplyr::count() %>% as.numeric()
    trial_count_go = trial_data %>% dplyr::filter(Trial_type != 0) %>% dplyr::count() %>% as.numeric()
    trial_count_nogo = trial_data %>% dplyr::filter(Trial_type == 0) %>% dplyr::count() %>% as.numeric()
    hit_percent = hits / trial_count_go
    # if Oddball experiment, FAs is out of total trials since they can always FA
    if (str_detect(analysis$type, pattern = "Oddball")) FA_percent = FAs / trial_count
    else if (trial_count_nogo > 0) FA_percent = FAs / trial_count_nogo
    else FA_percent = NA
    mean_attempts_per_trial = dplyr::summarise_at(trial_data, vars(Attempts_to_complete), mean, na.rm = TRUE)$Attempts_to_complete
    dprime = ifelse(trial_count_nogo == 0, NA, psycho::dprime(n_hit = hits,
                                                                           n_fa = FAs,
                                                                           n_miss = misses,
                                                                           n_cr = CRs,
                                                                           adjusted = TRUE) %>% .$dprime)

    if(analysis$type %in% c("Octave", "Training - Octave", "Training - Tone", "Training - BBN", "Training - Gap", "Training - Oddball",
                            "Oddball (Uneven Odds & Catch)", "Oddball (Uneven Odds)", "Oddball (Catch)", "Oddball (Background)", "Oddball (Standard)")) {
      TH_by_frequency_and_duration = NA
    } else {
      TH_by_frequency_and_duration = Calculate_Threshold()
    }

    if(analysis$type == "Octave") {
      FA_detailed = Calculate_FA_Detailed_Octave()
    } else if(str_detect(analysis$type, pattern = "Oddball")) {
      FA_detailed = Calculate_FA_Detailed_Oddball()
    } else {
      FA_detailed = NA
    }
    #overall_TH = Calculate_Threshold() #TODO overall calculation using trials archive
    reaction = Calculate_Reaction_Time() #NOTE this can take audible only (default false) or min time (default 0.015)


    stats = list(
      trial_count = trial_count,
      hits = hits,
      misses = misses,
      CRs = CRs,
      FAs = FAs,
      hit_percent = hit_percent,
      FA_percent = FA_percent,
      mean_attempts_per_trial = mean_attempts_per_trial,
      dprime = tibble(dprime),
      threshold = TH_by_frequency_and_duration,
      reaction = reaction,
      FA_detailed = FA_detailed
    )
    return(stats)
  }


  Check_Performance_Cutoffs <- function() {
    if (analysis$stats$trial_count < analysis$minimum_trials) { # this is ANALYSIS$minimum_trials which was set during analysis step, varies by type
      warn = paste0("Low trial count: ", analysis$stats$trial_count, " (cutoff is ", analysis$minimum_trials,")")
      warning(paste0(warn, "\n"))
      warnings_list <<- append(warnings_list, warn)
    }

    if (analysis$stats$hit_percent < user_settings$minimum_hit_percent) {
      warn = paste0("Low hit rate: ", round(analysis$stats$hit_percent * 100, digits = 1), "%")
      warning(paste0(warn, "\n"))
      warnings_list <<- append(warnings_list, warn)
    }

    if(!is.na(analysis$stats$FA_percent)) {
      if (analysis$stats$FA_percent > user_settings$maximum_FA_percent) {
        warn = paste0("High false alarm (FA) rate: ", round(analysis$stats$FA_percent * 100, digits = 1), "%")
        warning(paste0(warn, "\n"))
        warnings_list <<- append(warnings_list, warn)
      }
    }

    if (analysis$stats$mean_attempts_per_trial > user_settings$maximum_attempts_per_trial) {
      warn = paste0("High mean attempts per trial: ", round(analysis$stats$mean_attempts_per_trial, digits = 2))
      warning(paste0(warn, "\n"))
      warnings_list <<- append(warnings_list, warn)
    }
  }

  #TODO: this is untested!!!
  Check_Multipart_Run <- function () {
    Renumber_Complete_Blocks <- function() {
      issue_rows = run_archive %>%
        dplyr::filter(date == analysis$date && rat_name == run_properties$rat_name) %>%
        dplyr::select(UUID, time)

      r = trial_archive %>%
        dplyr::filter(UUID %in% issue_UUIDs$UUID) %>%
        dplyr::left_join(issue_rows, by = "UUID") %>%
        dplyr::arrange(time)

      block_status = r %>%
        dplyr::group_by(time, Block_number) %>%
        dplyr::summarise(Block_number = unique(Block_number), size = n(), complete = size == run_properties$stim_block_size) %>%
        dplyr::filter(complete) %>%
        dplyr::mutate(complete_block_number = row_number()) %>%
        dplyr::select(Block_number, complete_block_number, UUID)

      r = dplyr::left_join(r, block_status, by = "UUID") %>%
        dplyr::mutate(complete_block_number = dplyr::coalesce(complete_block_number.y, complete_block_number.x)) %>%
        dplyr::select(-complete_block_number.y, -complete_block_number.x)

      return(r)
    }

    is_multiple =
      run_archive %>%
      dplyr::filter(date == analysis$date && rat_name == run_properties$rat_name) %>%
      unique() %>%
      nrow() > 0

    if (is_multiple) {
      Renumber_Complete_Blocks()
      warn = paste0("Multiple runs detected for rat (", run_properties$rat_name, ") on this date (", analysis$date, ").",
                    "\n  Renumbering complete blocks.")
      warning(paste0(warn, "\n"))
      warnings_list <<- append(warnings_list, warn)

      #TODO: handle duplicating file assignment to extra runs
    }
  }

  Get_Rat_ID = function(check_name) {
    date = run_properties$creation_time %>% stringr::str_sub(1,8) %>% as.numeric()
    rats_with_name <- rat_archive %>%
      dplyr::filter(Rat_name %>% str_to_upper() == check_name %>% str_to_upper())
    if(nrow(rats_with_name) == 0) {
      stop(paste0("ABORT: No rats with name ", check_name, " found in archive."))
    } else {
      rat_ID = rats_with_name %>%
        dplyr::filter(start_date <= date) %>%
        dplyr::filter(date <= end_date | is.na(end_date)) %>% .$Rat_ID
      if(length(rat_ID) > 1) stop(paste0("ABORT: Overlapping rats on date ", date, " with name ", check_name, ". Cannot determine Rat ID."))
      if(length(rat_ID) == 0) stop("ABORT: No rats with name ", check_name, " were active on ", date, ".")
      return(rat_ID)
    }
  }

  Check_Weight <- function() {
    rat_weights = NULL
    if(!rlang::is_empty(run_archive)) {
      id = Get_Rat_ID(run_properties$rat_name)
      if(length(id) == 0) stop("ABORT: Unknown rat ID.")
      rat_weights =
        run_archive %>%
        dplyr::filter(rat_ID == id) %>%
        dplyr::select(date, weight)
    }

    date_asNumeric = run_properties$creation_time %>% stringr::str_sub(1,8) %>% as.numeric()
    date_asDate = paste0(stringr::str_sub(date_asNumeric, 1, 4), "-", stringr::str_sub(date_asNumeric, 5, 6), "-", stringr::str_sub(date_asNumeric, 7, 8)) %>% as.Date()

    if(old_file) {
      # need to fetch corresponding weight from old_excel_archive
      analysis$weight <<- old_excel_archive %>% dplyr::filter(Date == date_asDate & rat_name == run_properties$rat_name) %>% .$Weight
      if(is.na(analysis$weight)) {
        warn = paste0("No weight found in excel document for ", run_properties$rat_name, " on ", date_asDate, ".")
        warnings_list <<- append(warnings_list, warn)
        rat_weights = NULL
      }
    } else {
      #use the weight from the undergrad file's variable
      analysis$weight <<- weight
    }

    if(rlang::is_empty(rat_weights)) {
      warn = paste0("No weight data in run archive for ", run_properties$rat_name, "(", Get_Rat_ID(run_properties$rat_name), "). Is this rat new?")
      warning(paste0(warn, "\n"))
      warnings_list <<- append(warnings_list, warn)
      analysis$weight_change <<- 0
    } else {
      last_run = rat_weights %>% dplyr::filter(date < date_asNumeric & ! is.na(weight)) %>% dplyr::arrange(date) %>% tail(1)
      old_date = last_run$date
      old_weight = last_run$weight

      if(rlang::is_empty(old_weight)) {
        warn = paste0("No weights for ", run_properties$rat_name, "(", Get_Rat_ID(run_properties$rat_name), ") prior to ", date_asDate, ".")
        warning(paste0(warn, "\n"))
        warnings_list <<- append(warnings_list, warn)
        analysis$weight_change <<- 0
      } else {
        max_weight = max(rat_weights$weight, na.rm = TRUE) #TODO add a column to rat_archive called e.g. override_weight to use instead of true max weight for chunky bois, or calculate max in past month or something else
        analysis$weight_change <<- analysis$weight - old_weight  # negative if lost weight
        weight_change_percent = analysis$weight_change / old_weight # negative if lost weight
        weight_change_overall_percent = (analysis$weight - max_weight) / max_weight # negative if lost weight


        days_elapsed = date_asNumeric - old_date

        if(days_elapsed > 0) { # Don't want divide-by-zero for resumed runs
          weight_change_daily_percent = weight_change_percent/days_elapsed

          # Adjust the user setting to compensate for number of days elapsed
          adjusted_daily_threshold = user_settings$maximum_weight_change_daily_percent * (days_elapsed + 2) / (days_elapsed * 3)
          # The logic here is that if a single day dropping by 5% is bad,
          # But it's been, say, 3 days, how do we detect if we had an unrecorded 5% drop one of those days, and still lost weight on the other 2 days as well, we would definitely want to trigger the warning for.
          # We have 3 approaches:
            # No adjustment, warn on 5% crossed regardless of days -- but that warns too often, e.g. for a mere 1% per day for 5 consecutive days
            # No adjustment, warn on 5% crossed PER DAY regardless of days -- but that warns too inferquently, e.g. only if lost 5%+ every day (on average) for 5 consecutive days
            # Adjust threshold to be tighter the more days are being checked against, so that a pattern of consecutive losses or a loss large enough that one of the days might reasonably have been over threshold gets reported
          # We're doing the 3rd option here, with adjustment = threshold * (days+2)/(days*3)
          # A user setting daily threshold of 5% becomes, under that formula,
            # 5% for one day
            # 3.33% per day for 2 days (total drop of 6.7% over 2 days)
            # 2.78% per day for 3 days (total drop of 8.3% over 3 days)
            # 2.14% per day for 7 days (total drop of 15% over 7 days)
            # 1.90% per day for 14 days (total drop of 27% over 14 days)
            # To make it more conservative, add 1 to the two constants (e.g. days+3 / days*4)
            # To make it less conservative, subtract 1 from the constants (e.g. days+1 / days*2)

          # TODO future feature -- check e.g. last 7 weights against this same formula, so that we can notice strings of consecutive losses that were under the individual threshold

          if (-1 * weight_change_daily_percent > adjusted_daily_threshold) {
            warn = paste0("ACTION REQUIRED: Weight fell by more than ", 100*adjusted_daily_threshold, "% per day across ", days_elapsed, " day. (", old_weight, " -> ", analysis$weight, ").")
            warning(paste0(warn, "\n"))
            warnings_list <<- append(warnings_list, warn)
          }
        }
        if (-1 * weight_change_overall_percent > user_settings$maximum_weight_change_overall_percent) {
          warn = paste0("ACTION REQUIRED: Rat has lost more than ", 100*user_settings$maximum_weight_change_overall_percent, "% of maximum body weight. (", max_weight, " -> ", analysis$weight, ").")
          warning(paste0(warn, "\n"))
          warnings_list <<- append(warnings_list, warn)
        }
      }
    }
  }

  Add_to_Archives <- function() {
    Clear_Assignment <- function(rat_id) {
      cat("Rat... ")
      #store assignment for 24 hours for rerun if necessary
      rat_archive[rat_archive$Rat_ID == rat_id,]$Old_Assigned_Filename <<- rat_archive[rat_archive$Rat_ID == rat_id,]$Assigned_Filename
      rat_archive[rat_archive$Rat_ID == rat_id,]$Old_Assigned_Experiment <<- rat_archive[rat_archive$Rat_ID == rat_id,]$Assigned_Experiment
      rat_archive[rat_archive$Rat_ID == rat_id,]$Old_Assigned_Phase <<- rat_archive[rat_archive$Rat_ID == rat_id,]$Assigned_Phase
      rat_archive[rat_archive$Rat_ID == rat_id,]$Old_Assigned_Task <<- rat_archive[rat_archive$Rat_ID == rat_id,]$Assigned_Task
      rat_archive[rat_archive$Rat_ID == rat_id,]$Old_Assigned_Detail <<- rat_archive[rat_archive$Rat_ID == rat_id,]$Assigned_Detail

      rat_archive[rat_archive$Rat_ID == rat_id,]$Assigned_Filename <<- NA
      rat_archive[rat_archive$Rat_ID == rat_id,]$Assigned_Experiment <<- NA
      rat_archive[rat_archive$Rat_ID == rat_id,]$Assigned_Phase <<- NA
      rat_archive[rat_archive$Rat_ID == rat_id,]$Assigned_Task <<- NA
      rat_archive[rat_archive$Rat_ID == rat_id,]$Assigned_Detail <<- NA
    }

    Add_to_Run_Archive <- function(row_to_add) {
      cat("Run... ")
      run_archive <<- rbind(run_archive, row_to_add)
    }

    Add_to_Trial_Archive <- function(row_to_add) { # NOTE row_to_add is the row to add to the RUN archive, just used here to get uuid.
    cat("Trials... ")
    uuid = row_to_add$UUID
    experiment = row_to_add$assignment %>% .[[1]] %>% pluck("experiment")
    variable_name = paste0(experiment, "_archive")
    filename = paste0(projects_folder, variable_name, ".csv.gz")
      trials_to_write <<- trial_data
    }

    Construct_Run_Entry <- function() {
      date = run_properties$creation_time %>% stringr::str_sub(1,8) %>% as.numeric()
      time = run_properties$creation_time %>% stringr::str_sub(9,15) %>% as.numeric()

      # get excel data for run, if it exists
      if(old_file) {
        # need to fetch from old_excel_archive
        date_asDate = paste0(stringr::str_sub(date, 1, 4), "-", stringr::str_sub(date, 5, 6), "-", stringr::str_sub(date, 7, 8)) %>% as.Date()
        old_data = old_excel_archive %>% dplyr::filter(Date == date_asDate & rat_name == run_properties$rat_name)
        if(rlang::is_empty(old_data)) {
          warn = paste0("No data found in excel document for ", run_properties$rat_name, " on ", date, ".")
          warnings_list <<- append(warnings_list, warn)
        }
        observations = old_data$`Comments/Observations`
        assignment = list(
          assigned_file_name = old_data$Filename,
          experiment = old_data$Experiment,
          phase = old_data$Phase,
          task = old_data$Task,
          detail = old_data$Detail
        )
        scientist = old_data$scientist
        weightProblem = old_data$weightProblem
        rxnProblem = old_data$rxnProblem

        if(old_data$Invalid == "TRUE") invalid = "TRUE"

        if(rlang::is_empty(assignment$experiment) || rlang::is_empty(assignment$phase)) {
          warn = paste0("No experiment/phase found in excel document for ", run_properties$rat_name, " on ", date, ".")
          warnings_list <<- append(warnings_list, warn)
        }
      } else {
        #use the comments from the undergrad file's variable
        observations = observations   # redundant, just for clarity

        rat_id = Get_Rat_ID(run_properties$rat_name)
        if(length(rat_id) == 0) stop("ABORT: Unknown rat ID.")
        rat_data = rat_archive[rat_archive$Rat_ID == rat_id,]

        assignment = list(
          assigned_file_name = rat_data$Assigned_Filename, # analysis contains filename already imported from rat_archive, but we should read directly from rat_archive rather than reusing that
          experiment = rat_data$Assigned_Experiment,
          phase = rat_data$Assigned_Phase,
          task = rat_data$Assigned_Task,
          detail = rat_data$Assigned_Detail,
          comment = rat_data$Persistent_Comment
        )
        # also need to blank these fields from rat_archive after the import so they're never incorporated twice
        # --- must not do that here, instead do that down below, where we modify other archives so we don't blank until all chances for errors are past

      }

      # using data.frame instead of tibble automatically can unpack $stats into columns but still need concatenation for warnings_list
      #TODO: add duration probably to summary - if yes then handle it in supervisor as well, if no put it as big column?
      r = tibble(
        date = date,
        time = time,
        box = run_properties$box,    # TODO: calculated box from MAT file not external FOO.mat filename
        rat_name = run_properties$rat_name,
        rat_ID = Get_Rat_ID(run_properties$rat_name),
        weight = analysis$weight,

        file_name = analysis$computed_file_name,
        assignment = list(assignment),
        summary = list(run_properties$summary),

        stim_type = run_properties$stim_type,
        analysis_type = analysis$type,
        stats = list(analysis$stats),
        block_size = run_properties$stim_block_size,
        complete_block_count = trial_data$complete_block_number %>% max(na.rm = TRUE),

        # to be provided by shiny app (inheriting the defaults of "" for now), or was provided directly to a manual call of Process_File()
        scientist = scientist,
        comments = observations,    # observations taken during run
        weightProblem = weightProblem,
        rxnProblem = rxnProblem,

        warnings_list = list(warnings_list),
        omit_list = run_properties$omit_list,
        invalid = "",    # supervisor can manually mark runs as invalid, putting reasoning here
        UUID = run_properties$UUID
      )
      return(r)
    }


    # Add to Archive Workflow -------------------------------------------------

    row_to_add = Construct_Run_Entry()
    if (nrow(row_to_add) != 1) stop("ABORT: Problem building row to add to Run Archive.")

    cat("Archiving ")
    Add_to_Run_Archive(row_to_add)
    Add_to_Trial_Archive(row_to_add)
    if (!old_file) {Clear_Assignment(row_to_add$rat_ID)}
    return(row_to_add)
  }

# PROCESS FILE workflow ---------------------------------------------------------
  warnings_list <- list()
  rat_archive <<- fread(paste0(projects_folder, "rat_archive.csv"), na.strings = c("N/A","NA"))

  # load run's .mat file
  imported_data <- Import_Matlab(file_to_load)
  run_properties <- imported_data$run_properties
  trial_data <- imported_data$trial_data

  # generate UUID for run and make sure it's not already in run_archive
  if(Check_UUID()) {
    # identify analysis type
    analysis <- Identify_Analysis_Type()
    writeLines(glue("Analysis type: {analysis$type}"))
    if (use_shiny) shiny::showNotification(glue("{analysis$type}"))

    # summary statistics
    analysis$stats <- Calculate_Summary_Statistics()
    writeLines("Calculated run statistics.")
    if (use_shiny) shiny::showNotification(glue("Calculated run statistics."))

    # calculate canonical filename
    analysis$prepend_name <- FALSE
    analysis$computed_file_name <- Build_Filename()
    Check_Assigned_Filename()
    writeLines("Filename checks complete.")
    if (use_shiny) shiny::showNotification(glue("Filename checks complete."))

    # handle weight
    Check_Weight()
    writeLines("Weight checks complete.")
    if (use_shiny) shiny::showNotification(glue("Weight checks complete."))

    # check run performance against user-settings cutoffs
    Check_Performance_Cutoffs()
    # check run performance against past performance for this rat in this experimental phase
    # Check_Performance_Consistency
    writeLines("Performance checks complete.")
    if (use_shiny) shiny::showNotification(glue("Performance checks complete."))

    # commit to folding in - display warnings and get user input to override them (and submit to master dataframe), or give option to commit to invalid/storage-only dataframe
    # curate data prior to folding in, adding disqualifier flags etc (but omitted trials are always totally gone)
    # Report_Warnings_and_Confirm()
    row_added = Add_to_Archives()
    writeLines("")
    writeLines(paste0("Run ", row_added$UUID, " of ", row_added$rat_name, " (#", row_added$rat_ID, ") added to archives (in environment ONLY)."))
    #Generate_Chart()
  }

  # do analyses
  # pop up charts and stuff for undergrads to sign off on (where do the comments they provide on 'no' get saved? text file alongside individual exported graph image? dedicated df? master df in one long appended cell for all comments to graphs?)

  writeLines("") #TODO change all cats to writelines

  if(!exists("row_added")) {
    return(tibble(warnings_list = list(warnings_list),
                  UUID = run_properties$UUID))
  }
  return(row_added)
}

Generate_Weight_Graph <- function(rat_name, ratID) {
  #ratID = Get_Rat_ID(run_properties$rat_name)
  rat_runs = run_archive %>% dplyr::filter(rat_ID == ratID)
  rat_runs = rat_runs %>% mutate(date_asDate = lubridate::ymd(date))
  # thoughts - will want to standardize y axis to e.g. 80%-105% of baseline weight so that 'low' looks the same for everyone?
  # -- one problem with that is that if another rat's weight IS entered instead, it could be drastically above or below those bounds
  # I'm not bothering to figure out how to customize the axis labels right now.
  weight_graph =
    ggplot(rat_runs, aes(x = date_asDate, y = weight)) +
    geom_line(color = "grey", linewidth = 2) +
    geom_point(shape = 21, color = "black", fill = "#69b3a2", size = 5) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b%n%y") +
    ggtitle(paste0(rat_name, " Weight")) +
    theme_ipsum_es() +
    labs(x = NULL, y = NULL)
  #dev.new(width = 10, height = 6, noRStudioGD = TRUE) # This actually pops out. Size is ignored unless you tell RStudio not to help with the noRstudioGD argument.
  #print(weight_chart, vp = NULL) # vp is viewport https://ggplot2.tidyverse.org/reference/print.ggplot.html

  # writeLines("")
  # writeLines("Does this weight look OK? ")
  # weight_ok <- if(menu(c("Yes", "No")) == 1) TRUE else FALSE
  # problem <- NA
  # if (!weight_ok) {
  #   problem <- readline(prompt = "Please describe the problem: ")
  # }
  # initials <- readline(prompt = "Your initials: ")
  # dev.off()
  return(weight_graph)
}

Generate_Weight_Trials_Graph <- function(rat_name, ratID) {
  rat_runs <<- run_archive %>% dplyr::filter(rat_ID == ratID)
  # by mapping we don't need to unnest stats which is slow
  rat_runs = rat_runs %>% mutate(date_asDate = lubridate::ymd(date),
                                 trial_count = map_dbl(stats, ~.$trial_count)) %>%
    arrange(desc(date_asDate))
  run_date = head(rat_runs, 1)$date_asDate

  # min trials
  min_trials = pluck(user_settings, "minimum_trials",
                     arrange(rat_runs, desc(date)) %>% head(n = 1) %>% .$analysis_type)

  # max weight
  max_weight_runs = max(rat_runs$weight, na.rm = TRUE)
  max_weight_freefeed = dplyr::filter(rat_archive, Rat_ID == ratID)$Max_Weight
  max_weight = max(max_weight_runs, max_weight_freefeed)
  # annotate weight warnings
  rat_runs = rat_runs %>%
    mutate(weight_change = (weight - max_weight)/max_weight,
           weight_annotation = if_else(abs(weight_change) > 0.15, paste0(round((weight_change)*100, digits = 0), "%"), ""))

  has_annotations = nrow(filter(rat_runs, !is.na(weight_annotation))) >= 1

  # trial label
  trials_model = lm(trial_count ~ date_asDate, data = rat_runs)
  trials_label_x = predict(trials_model)[[1]]

  weight_and_trials_graph =
    rat_runs %>% filter(date > str_remove_all(run_date - 21, "-")) %>%
    ggplot(aes(x = date_asDate, y = weight)) +
    # min trials line
    geom_hline(yintercept = min_trials, linetype = "longdash") +
    geom_text(aes(x = min(date_asDate), y = min_trials - 5), label = "Minimum trials") +
    # trial trend line (Note we don't show all the trial point because they have great variation)
    geom_smooth(aes(x = date_asDate, y = trial_count),
                color = "lightsteelblue", fill = "lightsteelblue", linewidth = 2,
                se = FALSE, na.rm = TRUE, method = "lm", formula = y~x) +
    geom_text(aes(x = min(date_asDate), y = trials_label_x + 6), label = "Trial trend", color = "lightsteelblue") +
    # weight trend line
    geom_smooth(color = "grey", linewidth = 2,
                se = FALSE, na.rm = TRUE, method = "lm", formula = y~x) +
    # weight points
    geom_point(shape = 21, color = "black", fill = "#69b3a2", size = 5) +
    geom_text(aes(x = min(date_asDate), y = max_weight), nudge_x = -0.5, label = "Weight", color = "#69b3a2") +
    # # 3 most recent trial counts used to see if pulling up or down the trend
    geom_point(data = tail(arrange(rat_runs, date_asDate), n = 3), aes(y = trial_count), color = "black", show.legend = FALSE) +
    # annotations to weight points only if weight is 15%+ off max weight (Note we want this on top of other layers)
    {if(has_annotations) geom_text(aes(y = max_weight_runs + 6, label = weight_annotation), color = "darkred", size = 3)} +
    # lower the bottom of the y scale as it can be anchored to minimum trials and the label is bellow the line
    scale_y_continuous(expand = c(0.1, 0)) +
    scale_x_date(date_breaks = "4 day", date_minor_breaks = "2 day",
                 date_labels = "%b %e") +
    labs(title = glue("{unique(rat_runs$rat_name)} Weight & Trials (21d)"),
         x = "Date", y = "Weight & Trial Count") +
    theme_ipsum_es()

  return(weight_and_trials_graph)
}

Generate_Extra_Graphs <- function(rat_name, ratID) {
  source(paste0(projects_folder, "graphing unrolled.R"))
  return(Generate_Graph(rat_name, ratID))
}



Write_To_Archives <- function(row_added) {
  uuid = row_added$UUID
  experiment = row_added$assignment %>% .[[1]] %>% pluck("experiment")
  variable_name = paste0(experiment, "_archive")
  filename = paste0(projects_folder, variable_name, ".csv.gz") # trials archive

  run_archive <<- run_archive %>% rows_update(row_added, by = "UUID") # update run_archive with information from shiny

  fwrite(rat_archive, paste0(projects_folder, "rat_archive.csv"), row.names = FALSE)
  save(run_archive, file = paste0(projects_folder, "run_archive.Rdata"), ascii = TRUE, compress = FALSE)
  fwrite(cbind(trials_to_write, UUID = uuid), file = filename, append = file.exists(filename)) # tack UUID onto trials and add to archive, appending if the file already exists
  remove(trials_to_write, inherits = TRUE)

  writeLines(paste0("Run ", row_added$UUID, " of ", row_added$rat_name, " (#", row_added$rat_ID, ") saved to disk."))
}

# MAIN workflow ---------------------------------------------------------

# set up environment
InitializeMain()

#r = Process_File(file.choose(), name, weight, observations, exclude_trials)
#WriteToArchive(r)


