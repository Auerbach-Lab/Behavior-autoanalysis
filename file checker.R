
# Notes -------------------------------------------------------------------
# Assumes you have booth the 'called_for_file' and actual 'file_name'


# Break down file name ----------------------------------------------------

# Get 1st block of text which should be the go frequency (single or range)
file_name_frequency = gsub(pattern = "(^.*?)_.*$", replacement = "\\1", file_name)

# Get 1st block of text with dB. Note for octave files there are 2 of the blocks.
file_name_intensity = gsub(pattern = "^.*_(.*dB)_.*$", replacement = "\\1", file_name)


# Actual Stim -------------------------------------------------------------

# List of go sound frequencies
file_frequencies = unique(stim_master_list["Freq (kHz)"])

file_frequency_ranges = stim_master_list %>%
  dplyr::filter(`Inten (dB)` != -100) %>% # Remove No-Go from range
  dplyr::group_by(`Freq (kHz)`) %>%
  dplyr::summarise(range = unique(`Inten (dB)`)) # Get each unique dB

file_intensity_range_steps = file_frequency_ranges %>%
  dplyr::group_by(`Freq (kHz)`) %>%
  dplyr::summarise(min = min(range),
                   max = max(range),
                   steps = range - lag(range, default = first(range)),
                   .groups = 'keep') %>% # still grouped following this step, which is needed to remove the 1st row of each table that has a 0 step
  dplyr::slice(-1) %>% # Drop 1st row of each subtable
  .[!duplicated(.), ] # reduce to unique rows. Should be 1 row per frequency unless something is screwed up

# List of length of go sound - can be up to 3 values (50, 100, & 300) in our current file system
file_duration = unique(stim_master_list["Dur (ms)"])

file_response_window = unique(stim_master_list["Nose Out TL (s)"]) %>% as.numeric()
