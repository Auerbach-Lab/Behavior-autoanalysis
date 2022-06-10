
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

stim = current_file[["stim"]]
# make elements callable by name because somehow names are coming in as row.names
# This is because its importing as a matrix
names(stim) = row.names(stim)

file_settings = stim$para
names(file_settings) = row.names(file_settings)

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

background_dB = file_settings$BG.sound.inten

backround_file = file_settings$BG.sound["filepath",,]$filepath["filename",,]

background_type =
  backround_file %>%
  as.character() %>%
  stringr::str_remove(pattern = "^BG_", string = .) %>%
  stringr::str_remove(pattern = ".mat", string = .)

background_type =
  case_when(background_type == "PKN" ~ "Pink",
            background_type == "WN" ~ "White",
            background_type == "BBN" ~ "Broadband",
            TRUE ~ background_type) # This is a catch for a new type.
