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


# Import Run Data ---------------------------------------------------------
# takes actually run data file and yields:
# file_name,
source("~/GitHub/Behavior-autoanalysis/matlab import.R")


# Summary Calculation -----------------------------------------------------



