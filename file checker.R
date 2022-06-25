
# Notes -------------------------------------------------------------------
# Assumes you have booth the 'called_for_file' and actual 'file_name'


# Actual Stim -------------------------------------------------------------

# List of go sound frequencies
file_frequencies = unique(stim_master_list["Freq (kHz)"])

# Get ranges for each frequency
file_frequency_ranges = stim_master_list %>%
  dplyr::filter(`Inten (dB)` != -100) %>% # Remove No-Go from range
  dplyr::group_by(`Freq (kHz)`, `Delay (s)`, `Type`, `Repeat_number`) %>%
  dplyr::summarise(dB = unique(`Inten (dB)`), .groups = 'drop') # Get each unique dB

# Make summary data table with step size
file_summary = file_frequency_ranges %>%
  dplyr::group_by(`Freq (kHz)`, `Delay (s)`, `Type`) %>%
  dplyr::summarise(min = min(dB),
                   max = max(dB),
                   steps = dB - lag(dB, default = first(dB)),
                   .groups = 'keep') %>% # still grouped following this step, which is needed to remove the 1st row of each table that has a 0 step
  dplyr::slice(-1) %>% # Drop 1st row of each sub-table
  .[!duplicated(.), ] # reduce to unique rows. Should be 1 row per frequency unless something is screwed up

# cleanup the duplicated table
rm(file_frequencies)


# List of length of go sound - can be up to 3 values (50, 100, & 300) in our current file system
duration = unique(stim_master_list["Dur (ms)"])

# Get response window
response_window = unique(stim_master_list["Nose Out TL (s)"]) %>% as.numeric()


# Check dB step size ------------------------------------------------------

dB_step_size = unique(file_summary$steps)

# Check for mismatched step size
if (length(unique(file_summary$steps)) != 1) {
  warning(paste0("Missmatched step size in file.\nAction Required: Correct ", file_name, "\n"))
  Warnings = append(Warnings, paste0("Action Required: Missmatched step size in file ", file_name))
}

# Analysis Type ----------------------------------------------------------
# Automatically attempt to determine analysis type. This should be checked against
# master/power user data table.

# For tonal files (octaves, or mainly 4-32kHz)
if (stim_type == "tone") {
  # Determine if it has custom ranges (i.e. not all frequencies have the same range)
  if ((length(unique(file_summary$min)) != 1 | length(unique(file_summary$max)) != 1)) {analysis_type = "custom tone"}
  else (
    # Determine if octave file (in that case one of the normal intensity (dB) should be 0 or non-rewarded)
    # Note that for each type 1 and type 0 the min & max should be equal (i.e. one intensity) but not necessarily between type 1 & 0
    if (any(file_summary$Type == 0)) {analysis_type = "octave"}
    else (
      # Determine if all frequencies have the same range
      if ((length(unique(file_summary$min)) == 1 & length(unique(file_summary$max)) == 1)) {analysis_type = "standard tone"}
      else (stop("Unknown tonal file type."))
    )
  )
}

# For broadband files (training or otherwise)
if (stim_type == "BBN") {
  # Determine if training
  if (file_summary$min == file_summary$max) {analysis_type = "BBN Training"}
  else (analysis_type = "BBN")
}

# For Oddball files (training or otherwise)
# TODO: This needs rewriting into a proper if else as "oddball with catch trials" also errors at the stop.
if (stim_type == "train") {
  # Determine if catch trials
  if (any(file_summary$Type == 0)) {analysis_type = "Oddball with catch trials"}
  # Determine if even odds
  if (file_frequency_ranges %>%
      dplyr::filter(Type != 0) %>% # Remove catch trials
      dplyr::select(Repeat_number) %>%
      unique() %>%
      length() != 1) {analysis_type = "Oddball uneven trial odds"}
  # Check for odds and catch trials
  if (any(file_summary$Type == 0) & # Has catch trials
      # Has uneven trial odds
      file_frequency_ranges %>%
      dplyr::filter(Type != 0) %>% # Remove catch trials
      dplyr::select(Repeat_number) %>%
      unique() %>%
      length() != 1) {analysis_type = "Oddball with uneven trial & catch trials"}
  else (stop("Unknown Oddball file type."))
}

if (!(exists("analysis_type"))) {stop("\nUnknown file type. Can not proceed with analysis")}

# Build 'real' file name --------------------------------------------------


# Check File name vs. file ------------------------------------------------


# Expected vs. file -------------------------------------------------------


# Variable cleanup --------------------------------------------------------
# Remove temp variables from the environment as they shouldn't be needed again

rm(list = c("stim_master_list"))


