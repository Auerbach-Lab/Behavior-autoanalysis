#TODO Should there be a sanity check here with the rat's name?
# reasoning -- if an undergrad puts in information for blue1 but doubleclicks the file for red47...
# as long as there's not some (other) catastrophic error that would cause a halt on red47 ANYWAY ...
# ... then the data will get added with wrong comments, weight, excludelist for red47,
# and the undergrad WON'T EVEN KNOW something went wrong



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
