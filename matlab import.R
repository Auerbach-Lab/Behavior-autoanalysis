
# Loading current file ----------------------------------------------------
current_file = readMat("C:/Users/Noelle/Box/Behavior Lab/Projects (Behavior)/TTS/data/20220609/Green1_4-32kHz_30-90dB_50ms_8s_1s_TR100ms_20220609-101834_BOX#001.mat")

# File Breakdown ----------------------------------------------------------

# Table of reaction times with # of stim TODO: - decode stim to frequency (kHz),
# loudness (dB), Duration (ms) This may not be best place to pull from; looking
# in stim there may be an already decoded table
run_data_encoded = current_file$result

stim = current_file[["stim"]]
# make elements callable by name because somehow names are coming in as row.names
names(stim) = row.names(stim)

# Get file name -----------------------------------------------------------

file_name =
  stim[["name"]] %>%
  as.character() %>%
  # remove excess info (i.e. .mat and then file location)
  stringr::str_remove(pattern = ".mat @ .*$", string = .)

file_location =
  stim[["name"]] %>%
  as.character() %>%
  # remove excess info (i.e. .mat and then file location)
  stringr::str_remove(pattern = "^.*?.mat @ ", string = .)


# Background noise --------------------------------------------------------

