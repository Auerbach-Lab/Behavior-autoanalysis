
# Notes -------------------------------------------------------------------
# Assumes you have booth the 'called_for_file' and actual 'file_name'


# Actual Stim -------------------------------------------------------------

# List of go sound frequencies
file_frequencies = unique(stim_master_list["Freq (kHz)"])

file_frequency_ranges = stim_master_list %>%
  dplyr::filter(`Inten (dB)` != -100) %>% # Remove No-Go from range
  dplyr::group_by(`Freq (kHz)`, `Delay (s)`, `Type`) %>%
  dplyr::summarise(dB = unique(`Inten (dB)`), .groups = 'drop') # Get each unique dB

file_summary = file_frequency_ranges %>%
  dplyr::group_by(`Freq (kHz)`, `Delay (s)`, `Type`) %>%
  dplyr::summarise(min = min(dB),
                   max = max(dB),
                   steps = dB - lag(dB, default = first(dB)),
                   .groups = 'keep') %>% # still grouped following this step, which is needed to remove the 1st row of each table that has a 0 step
  dplyr::slice(-1) %>% # Drop 1st row of each sub-table
  .[!duplicated(.), ] # reduce to unique rows. Should be 1 row per frequency unless something is screwed up

# Check for mismatched step size
if (length(unique(file_summary$steps)) != 1) {
  warning(paste0("Missmatched step size in file.\nAction Required: Correct ", file_name, "\n"))
  Warnings = append(Warnings, paste0("Action Required: Missmatched step size in file ", file_name))
}

# cleanup the duplicated table
rm(file_frequencies)

# List of length of go sound - can be up to 3 values (50, 100, & 300) in our current file system
duration = unique(stim_master_list["Dur (ms)"])

response_window = unique(stim_master_list["Nose Out TL (s)"]) %>% as.numeric()


# Build 'real' file name --------------------------------------------------

# For standard tonal files
if (analysis_type == "tone range") {
  NULL
}

# For standard broadband files
if (analysis_type == "BBN") {
  NULL
}

# For standard broadband files
if (analysis_type == "BBN training") {
  NULL
}

# For Oddball files
if (analysis_type == "Oddball") {
  NULL
}

# Check File name vs. file ------------------------------------------------


# Expected vs. file -------------------------------------------------------


# Variable cleanup --------------------------------------------------------
# Remove temp variables from the environment as they shouldn't be needed again

rm(list = c("stim_master_list"))


