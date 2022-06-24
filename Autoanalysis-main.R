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
library(R.matlab); library(readxl); library(tidyverse); library(dplyr); library(tidyr)

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


# Determine analysis type -------------------------------------------------
# File and range to determine the expected analysis. This should be verified against the master experiment.
source("~/GitHub/Behavior-autoanalysis/analysis typer.R")


# File Check --------------------------------------------------------------
# Checks actual file details against the expected/called for file.
source("~/GitHub/Behavior-autoanalysis/file checker.R")


# Summary Calculation -----------------------------------------------------


# Error Log Handling ------------------------------------------------------

if (length(Warnings) == 0) {Warnings = "None"}



