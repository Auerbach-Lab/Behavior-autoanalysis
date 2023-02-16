
# User set variables ------------------------------------------------------
Trial_archive = "Tsc2-LE"


# Functions ---------------------------------------------------------------
Process_File <- function(df) {
  Import_Matlab <- function(file_to_load) {
    Unlist_Matlab_To_Dataframe <- function(li) {
      return(t(apply(li, 1, unlist)) %>% as.data.frame())
    }
    
    Get_Stim_Encoding_Table <- function() {
      
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
  
      return(stim_encoding_table)
    }
    
    Get_Run_Properties <- function() {
      Get_Stim_Filename <- function() {
        r =
          trial_collection$name[1] %>%
          # remove excess info (i.e. .mat and then file location)
          stringr::str_remove(pattern = ".mat @ .*$", string = .)
        cat("Stim file:", r, sep = "\t", fill = TRUE)
        return(r)
      }
      
      Get_Rat_Name <- function() {
        # greedy group: (.*) to strip off as much as possible
        # then the main capture group which contains
        # lookbehind for either \ (escaped once because R, and then again cause regex, to \\\\) or / character, specified length 1 because r: (?<=[\\/]{1})
        # important! \\\\ is specific to R, for testing this pattern in e.g. RegExr you have to use \\ but remember to change it back to \\\\ for r!
        # capture of the rat name, lazy to avoid underscores: .+?
        # lookahead for a _: (?=_)
        print(file_to_load)
        r = stringr::str_match_all(file_to_load, pattern="(.*)((?<=[\\\\/]{1}).+?(?=_))") %>%
          unlist(recursive = TRUE) %>%
          tail (n = 1)
        
        r_compare = r %>% str_replace_all(" ", "") %>% str_to_lower()
        if(! ignore_name_check) {
          name_compare = name %>% str_replace_all(" ", "") %>% str_to_lower() # from undergraduate.R
        }
        
        if (rlang::is_empty(r)) stop("ERROR: system filename improper: ", file_to_load)
        if (!ignore_name_check && r_compare != name_compare) stop(paste0("ABORT: Rat name given (", name_compare, ") does not match chosen file (", r_compare, ")."))
        
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
        else {
          warn = paste0("WARNING: Multiple non-oddball stim types: ", stim_type)
          warning(paste0(warn, "\n"))
          warnings_list <<- append(warnings_list, warn)
          stim_type = stim_encoding_table %>% filter(Type == "1") %>% .$`Stim Source`
        }
      } else {
        stim_type = stim_type %>% as.character()
      }
      
      r = list(
        rat_name = Get_Rat_Name(),
        
        stim_filename = Get_Stim_Filename(),
        stim_block_size = sum(stim_encoding_table["Repeat_number"]),
        stim_type = stim_type,
        
        background_dB = background_dB,
        background_file = background_file,
        background_type = background_type,
        
        # Maximum number of back to back no go trials (0 or blank is infinite)
        nogo_max_touching = run_properties$no.go.trial.max.num[[1]],
        
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
                                    by = "Stim_ID", all.x = TRUE)
      trial_data = dplyr::bind_cols(trial_data, Get_Delay_DF(trial_data))
      
      #TODO: detect same day same rat data, renumber blocks in this list AND in master dataframe to continuous chronological order
      #see also: CheckMultipartRun
      block_list = rep(1:ceiling(nrow(trial_data)/run_properties$stim_block_size), each = run_properties$stim_block_size)
      
      trial_data = trial_data %>% dplyr::mutate(Trial_number = row_number(),
                                                Block_number = head(block_list, n = nrow(trial_data)))
      
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
      if (! is.na(exclude_trials)) omit_list = Query_Omitted_Trials()
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
  
  Get_Rat_ID = function(check_name) {
    date = run_properties$creation_time %>% stringr::str_sub(1,8) %>% as.numeric()
    rats_with_name <- rat_archive %>%
      dplyr::filter(Rat_name == check_name)
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
  
  Check_UUID <- function() {
    Build_UUID <- function() {
      
      trial1_start_time = trial_data$`Time_since_file_start_(s)`[1]
      trial1_delay_time = trial_data$`Delay (s)`[1]
      current_file_UUID = paste0(run_properties$creation_time, "_", trial1_start_time, "_", trial1_delay_time)
      
      return(current_file_UUID)
    }
    
    Is_New_UUID <- function(uuid) {
      is_old = uuid %in% trials_UUIDs$UUID
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
  
  Add_to_Trial_Archive <- function(rat_id, current_file_UUID) { # NOTE row_to_add is the row to add to the RUN archive, just used here to get uuid.
    cat("Trials... ")
    uuid = current_file_UUID
    fwrite(cbind(trial_data, UUID = uuid), file = filename, append = file.exists(filename)) # tack UUID onto trials and add to archive, appending if the file already exists
    cat(glue("appended to {Trial_archive}\n"))
  }
  

# WORKFLOW ----------------------------------------------------------------
  
  df = as_tibble_row(df)
  
  rat_ID = df$rat_ID
  name = df$rat_name
  date = lubridate::as_date(df$date)
  exclude_trials = df$omit_list
  ignore_name_check = FALSE
  writeLines(glue('Pick file for {name} on {date}'))
  file_to_load <- choose.files(default = glue("Z:/Daily Matlab files/{date} {name}"))
  
  # load run's .mat file
  imported_data <- Import_Matlab(file_to_load)
  run_properties <- imported_data$run_properties
  trial_data <- imported_data$trial_data
  
  filename = paste0(Trial_archive, "_archive.csv.gz")
  trials_UUIDs = fread(paste0(projects_folder, filename), select = "UUID")
  current_file_UUID <- Check_UUID()
  
  rat_id = Get_Rat_ID(run_properties$rat_name)
  if (rat_ID != rat_id) stop("ABORT: rat IDs do not match.")
  if (length(rat_id) == 0) stop("ABORT: Unknown rat ID.")
  
  cat("Archiving ")
  Add_to_Trial_Archive(rat_id, current_file_UUID)
  writeLines("Done\n")
  
}

missing_files = fread("missing.csv")
apply(missing_files, 1, Process_File) # 1 sets it to apply by rows not columns

