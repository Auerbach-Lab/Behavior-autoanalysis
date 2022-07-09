
# Loading current file ----------------------------------------------------
# # No Background
# current_file_nobg = readMat("C:/Users/Noelle/Box/Behavior Lab/Projects (Behavior)/TTS/data/20220609/Green1_4-32kHz_30-90dB_50ms_8s_1s_TR100ms_20220609-101834_BOX#001.mat")
# # Background sound
# current_file_bg = readMat("C:/Users/Noelle/Box/Behavior Lab/Projects (Behavior)/TTS/data/20220609/Green2_4-32kHz_30-90dB_50ms_16s_1s_TR100ms_BG_PNK_30dB_20220609-101835_BOX#002.mat")

current_file = current_file_bg


# File Breakdown ----------------------------------------------------------
stim = current_file$stim[,,1]

file_settings = stim$para[,,1]

# Get file name -----------------------------------------------------------

file_name =
  stim$name[1] %>%
  # remove excess info (i.e. .mat and then file location)
  stringr::str_remove(pattern = ".mat @ .*$", string = .)

file_location =
  stim$name[1] %>%
  # remove excess info (i.e. the name of the .mat file)
  stringr::str_remove(pattern = "^.*?.mat @ ", string = .)


# Background noise --------------------------------------------------------

# does not work with is.NULL
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

  background_type =
    case_when(background_type == "PKN" ~ "Pink",
              background_type == "WN" ~ "White",
              background_type == "BBN" ~ "Broadband",
              TRUE ~ background_type) # This is a catch for a new type.

}


# Extra file settings -----------------------------------------------------

# Maximum number of back to back no go trials (0 or blank is infinite)
no_go_max_touching = file_settings$no.go.trial.max.num[[1]]


# DTW setting in the behavior program or TR in file names
# DTW = detect time window & TR = Trigger
# In either case, this is not to be confused with the response window
#   i.e. how long the rat has post trial start to respond
# This is how long the nose must remain out to be counted as 'withdraw' or
# response. Response time is for the withdraw.
trigger_senesitivity = file_settings$detect.time.win[[1]]

# Nose light operational?
nose_light = file_settings$nose.light[[1]] %>% as.logical()


# MATLAB Summary ----------------------------------------------------------
# This is the historic summary that we write down. This should be sanity checked
# against the actual data

results_total_trials = current_file$final.result[,,1]$go.trial.num[1] + current_file$final.result[,,1]$no.go.trial.num[1]
results_hits = current_file$final.result[,,1]$hit.num[1]
results_misses = current_file$final.result[,,1]$miss.num[1]
results_CR = current_file$final.result[,,1]$CR.num[1]
results_FA = current_file$final.result[,,1]$FA.num[1]


# Decode result table -----------------------------------------------------
# Table of reaction times with # of stim

# stim_master_list = stim$source.list

# stim$source.list isn't coming in as a table (for these test files it should be 9 wide by 29 long)
# runs down the columns (check item 29, stim$source.list[[29]] == 1,28 in MATLAB)
# This is because in MATLAB it is type cell (an array)
# Brian has made a fix that can be run manually and submitted a pull request for R.matlab

stim_master_list = read_csv("~/GitHub/Behavior-autoanalysis/source_list.csv", col_names = FALSE, show_col_types = FALSE)
names(stim_master_list) = append(unlist(stim$stim.tag.list), "Repeat_number", after = 0)
# Add identifying number for decoding)
stim_master_list = dplyr::mutate(stim_master_list, "Stim_ID" = row_number())

# Get stim variables automatically
stim_block_size = sum(stim_master_list["Repeat_number"])
stim_type = unique(stim_master_list["Stim Source"]) %>% as.character()

run_data_encoded = data.frame(current_file$result)

# The MATLAB file has 2 extra columns for some unknown reason
if (all(run_data_encoded[7:8] != "0")) {
  stop("What are these columns storing?")
  } else {
  run_data_encoded = run_data_encoded[1:6]
  }

names(run_data_encoded) = list("Time_since_file_start_(s)", "Stim_ID", "Tial_type", "Attempts_to_complete", "Response", "Reaction_(s)")

run_data_encoded = run_data_encoded %>%
                   dplyr::mutate(Response = dplyr::case_when(Response == 1 ~ "Hit",
                                                             Response == 2 ~ "Miss",
                                                             Response == 3 ~ "FA",
                                                             Response == 4 ~ "CR",
                                                             TRUE ~ "ERROR"))

run_data = dplyr::left_join(x = run_data_encoded,
                            y = dplyr::select(stim_master_list, -Repeat_number),
                            by = "Stim_ID", all.x = TRUE)


# File sanity checks ------------------------------------------------------
# Ensure that the MATLAB summary and the calculated summary match

# Calculate the summary statistics
total_trials = run_data %>% dplyr::count() %>% as.numeric()
hits_calc = run_data %>% dplyr::filter(Response == "Hit") %>% dplyr::count() %>% as.numeric()
misses_calc = run_data %>% dplyr::filter(Response == "Miss") %>% dplyr::count() %>% as.numeric()
CRs_calc = run_data %>% dplyr::filter(Response == "CR") %>% dplyr::count() %>% as.numeric()
FAs_calc = run_data %>% dplyr::filter(Response == "FA") %>% dplyr::count() %>% as.numeric()

# Check calculated stats against MATLAB summary stats
if (total_trials != results_total_trials) stop("Trial count miss-match")
if (hits_calc != results_hits) stop("Hit count miss-match")
if (misses_calc != results_misses) stop("Misses count miss-match")
if (CRs_calc != results_CR) stop("Correct Reject (CR) count miss-match")
if (FAs_calc != results_FA) stop("False Alarm (FA) count miss-match")

# Variable cleanup --------------------------------------------------------
# Remove temp variables from the environment as they shouldn't be needed again

# cleanup MATLAB summary
rm(list = c("results_total_trials", "results_hits", "results_misses", "results_CR", "results_FA"))

# cleanup extraneous copies of the current file
rm(list = c("current_file", 'file_settings', 'run_data_encoded', 'stim'))

