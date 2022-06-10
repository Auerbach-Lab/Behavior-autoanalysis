
# Loading current file ----------------------------------------------------
# No Background
current_file_nobg = readMat("C:/Users/Noelle/Box/Behavior Lab/Projects (Behavior)/TTS/data/20220609/Green1_4-32kHz_30-90dB_50ms_8s_1s_TR100ms_20220609-101834_BOX#001.mat")
# Background sound
current_file_bg = readMat("C:/Users/Noelle/Box/Behavior Lab/Projects (Behavior)/TTS/data/20220609/Green2_4-32kHz_30-90dB_50ms_16s_1s_TR100ms_BG_PNK_30dB_20220609-101835_BOX#002.mat")


current_file = current_file_bg


# File Breakdown ----------------------------------------------------------

# Table of reaction times with # of stim TODO: - decode stim to frequency (kHz),
# loudness (dB), Duration (ms) This may not be best place to pull from; looking
# in stim there may be an already decoded table
run_data_encoded = current_file$result

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

background_dB = file_settings$BG.sound.inten[1]

backround_file = file_settings$BG.sound["filepath",,]$filepath["filename",,]$filename[1]

background_type =
  backround_file %>%
  stringr::str_remove(pattern = "^BG_", string = .) %>%
  stringr::str_remove(pattern = ".mat", string = .)

background_type =
  case_when(background_type == "PKN" ~ "Pink",
            background_type == "WN" ~ "White",
            background_type == "BBN" ~ "Broadband",
            TRUE ~ background_type) # This is a catch for a new type.

# Extra file settings -----------------------------------------------------

# Maximum number of back to back no go trials (0 or blank is infinite)
no_go_max_touching = file_settings$no.go.trial.max.num[[1]]


# DTW setting in the behavior program or TR in file names
# DTW = detect time window & TR = Trigger
# In either case, this is not to be confused with the response window
#   i.e. how long the rat has post trial start to respond
trigger_senesitivity = file_settings$detect.time.win[[1]]

# Nose light operational?
nose_light = file_settings$nose.light[[1]] %>% as.logical()


# Matlab Summary ---------------------------------------------------------- This
# is the historic summary that we write down. This should be sanity checked
# against the actual data

results_total_trials = current_file$final.result[,,1]$go.trial.num[1] + current_file$final.result[,,1]$no.go.trial.num[1]
results_hits = current_file$final.result[,,1]$hit.num[1]
results_misses = current_file$final.result[,,1]$miss.num[1]
results_CR = current_file$final.result[,,1]$CR.num[1]
results_FA = current_file$final.result[,,1]$FA.num[1]



