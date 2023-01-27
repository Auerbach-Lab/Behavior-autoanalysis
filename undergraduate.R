# Rat name
# Capitalization and spaces do not matter
# Example: rat_name = "Orange11"
name = "Blue4"

# Trials to exclude from the next file.
# A comma-separated list of numbers or ranges inside quotation marks.
# Example: exclude_trials = "2, 120-126, 201"
# Use empty quotation marks if there are no trials to exclude.
# Example: exclude_trials = ""
exclude_trials = ""

# Rat's weight, in grams.
# Example: weight = 360
# **IGNORED** if old_file is TRUE.
weight = 583

# Observations made during the run, in quotation marks.
# Example: observations = "Good hits. Good misses. Only real FAs. 2 CRs back to back. Break from 35-47m, with no jams."
observations = "good hits"


projects_folder = "Z:/Behavior-autoanalysis/"
source(paste0(projects_folder, "main.R"))
