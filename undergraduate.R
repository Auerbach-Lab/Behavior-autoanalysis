
# Trials to exclude from the next file.
# A comma-separated list of numbers or ranges inside quotation marks.
# Example: exclude_trials = "2, 120-126, 201"
# Use empty quotation marks if there are no trials to exclude.
# Example: exclude_trials = ""
exclude_trials = ""

# Is there data about this file in the (old) excel sheets
# A boolean value (TRUE or FALSE)
# Example: old_file = TRUE
old_file = TRUE  #TODO: grab weight and assignment and experiment info from old excel sheet

# Rat's weight, in grams.
# Example: weight = 360
# **IGNORED** if old_file is TRUE.
weight = 200

# Observations made during the run, in quotation marks.
# Example: observations = "Good hits. Good misses. Only real FAs. 2 CRs back to back. Break from 35-47m, with no jams."
# **IGNORED** if old_file is TRUE.
observations = "Sample observation."



source("A:/Coding/Behavior-autoanalysis/main.R")
