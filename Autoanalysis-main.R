# NOTES -------------------------------------------------------------------
# Much of this is hardcoded based on the file structure of the matlab files used
# by the Auerbach lab. Altering these WILL BREAK this script.

# The individual files should be organized in to folders by day, with the raw
# data CSV's being in another /export subfolder. This is determined by the
# current Behavior_Analysis matlab script.


# Enviornment Control -----------------------------------------------------

# TODO: get renv working. See https://rstudio.github.io/renv/articles/renv.html


# Package loading ---------------------------------------------------------

# data loading/manipulation
library(R.matlab);
source("~/GitHub/Behavior-autoanalysis/matlab import.R")
library(readxl); library(tidyverse); library(dplyr); library(tidyr)

# Analysis
library(psych); library(psycho);

# Data visualization
library(ggplot2); library(forcats);

# Exporting data


# Global Variables --------------------------------------------------------

# The folder where the project subfolders are:
MainFolder = 'C:/Users/Noelle/Box/Behavior Lab/Projects (Behavior)'

# Minimum number of blocks of trials to keep (vs. discard day) not counting the
# 1st block which will be dropped
min_blocks = 5

# Sensitivity cutoff for determining hearing thresholds
TH_cutoff = 1.5


# Working directory -------------------------------------------------------
setwd(MainFolder)


# Initiate Error log list -------------------------------------------------
Warnings = list()


# Import Run Data ---------------------------------------------------------
# takes actually run data file and yields:
#   file_name, stim_type, and other details on file (no_go_max_touching, nose_light, trigger_sensitivity)
#   background_type & background_dB
#   total_trials, hits_calc, misses_calc, CRs_calc, FAs_calc
#   run_data (decoded with frequency, intensity, attempt #, rxn time + others)
source("~/GitHub/Behavior-autoanalysis/matlab import.R")


# File Check --------------------------------------------------------------
# Checks actual file details against the expected/called for file.
# File and range to determine the expected analysis. This should be verified against the master experiment.
source("~/GitHub/Behavior-autoanalysis/file checker.R")


# File Specific Analysis --------------------------------------------------

if (analysis_type = "standard tone") {source("~/GitHub/Behavior-autoanalysis/standard tone handler.R")
} else if (analysis_type = "custom tone") {source("~/GitHub/Behavior-autoanalysis/custom tone handler.R")
} else if (analysis_type = "octave training") {source("~/GitHub/Behavior-autoanalysis/octave training handler.R")
} else if (analysis_type = "octave") {source("~/GitHub/Behavior-autoanalysis/octave handler.R")
} else if (analysis_type = "BBN Training") {source("~/GitHub/Behavior-autoanalysis/BBN Training handler.R")
} else if (analysis_type = "BBN duration") {source("~/GitHub/Behavior-autoanalysis/BBN duration handler.R")
} else if (analysis_type = "BBN") {source("~/GitHub/Behavior-autoanalysis/BBN handler.R")
} else if (analysis_type = "Oddball with odds & catch trials") {source("~/GitHub/Behavior-autoanalysis/Oddball odds catch handler.R")
} else if (analysis_type = "Oddball with uneven trial odds") {source("~/GitHub/Behavior-autoanalysis/Oddball odds handler.R")
} else if (analysis_type = "Oddball with catch trials") {source("~/GitHub/Behavior-autoanalysis/Oddball catch handler.R")
} else {stop("Unknown analysis type. Can not continue")}


# Summary Calculation -----------------------------------------------------


# Error Log Handling ------------------------------------------------------

if (length(Warnings) == 0) {Warnings = "None"}



