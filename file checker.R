
# Checking Stim File ------------------------------------------------------
# Summarizes stim file and selects analysis type
# Summarizes kHz ranges, dB step size, durations, lockout window, & delay range

# Duplicate Stim Check ----------------------------------------------------

# Check that all stims are unique
stim_not_unique = stim_master_list %>%
  dplyr::group_by(`Freq (kHz)`, `Inten (dB)`, `Dur (ms)`, `Type`) %>%
  dplyr::filter(n() > 1) %>%
  summarize(n = n(), .groups = 'drop')

# Warning
if (nrow(stim_not_unique) != 0) {
  warning(paste0("Repeated stim.\nAction Required: Correct ", file_name, "\n"))
  Warnings = append(Warnings, paste0("Action Required: Multiple (", nrow(stim_not_unique), ") identical stims in ", file_name))
}

rm(stim_not_unique)

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

# Get Lock Out time
Lockout = unique(stim_master_list$`Time Out (s)`)[unique(stim_master_list$`Time Out (s)`) > 0]

# Get Delay
Delay = unique(stim_master_list$`Delay (s)`)

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

# Tone file properties tests ---------------------------------------------
# Determine if it has custom ranges (i.e. not all frequencies have the same range)
has_different_dB_ranges_for_frequencies = length(unique(file_summary$min)) != 1 | length(unique(file_summary$max)) != 1
# Determine if octave file (in that case one of the normal intensity (dB) should be 0 or non-rewarded)
# Note that for each type 1 and type 0 the min & max should be equal (i.e. one intensity) but not necessarily between type 1 & 0
has_audible_NoGo = any(file_summary$Type == 0)
# Test octave files have multiple types of No Go trials in the audible range
has_more_than_one_NoGo = length(unique(dplyr::filter(file_summary, Type == 0)$Type)) > 1


# For tonal files (octaves, or mainly 4-32kHz)
if (stim_type == "tone") {
  if (has_different_dB_ranges_for_frequencies) {analysis_type = "custom tone"}
  else if (has_audible_NoGo & has_more_than_one_NoGo) {analysis_type = "octave"}
  else if (has_audible_NoGo & !has_more_than_one_NoGo) {analysis_type = "octave training"}
  else if (!has_different_dB_ranges_for_frequencies) {analysis_type = "standard tone"}
  else (stop("Unknown tonal file type."))
}

rm(list = c("has_different_dB_ranges_for_frequencies", "has_audible_NoGo", "has_more_than_one_NoGo"))



# BBN file properties tests -----------------------------------------------

has_one_dB = file_summary$min == file_summary$max
has_multiple_durations = nrow(duration) > 1

# For broadband files (training or otherwise)
if (stim_type == "BBN") {
  # Determine if training
  if (has_one_dB) {analysis_type = "BBN Training"}
  else if (has_multiple_durations) {analysis_type = "BBN duration"}
  else (analysis_type = "BBN")
}

rm(list = c("has_one_dB", "has_multiple_durations"))


# Oddball file properties tests -------------------------------------------

# Determine if catch trials
has_catch_trials = any(file_summary$Type == 0)
# Determine if even odds
has_uneven_trial_odds = file_frequency_ranges %>%
                          dplyr::filter(Type != 0) %>% # Remove catch trials
                          dplyr::select(Repeat_number) %>%
                          unique() %>%
                          length() != 1

# For Oddball files (training or otherwise)
if (stim_type == "train") {
  if (has_catch_trials & has_uneven_trial_odds) {analysis_type = "Oddball with odds & catch trials"}
  else if (has_uneven_trial_odds) {analysis_type = "Oddball with uneven trial odds"}
  else if (has_catch_trials) {analysis_type = "Oddball with catch trials"}
  else (stop("Unknown Oddball file type."))
}

rm(list = c("has_catch_trials", "has_uneven_trial_odds"))

# Analysis Type = Undefined -----------------------------------------------

invisible(
  if (!exists("analysis_type")) {stop("\nUnknown file type. Can not proceed with analysis")}
  else (cat("Proceeding with analysis:", stringr::str_to_title(analysis_type), sep = "\t"))
)

