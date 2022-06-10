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
library(R.matlab); library(readxl); library(tidyverse); library(magrittr); library(dplyr); library(tidyr);

# Analysis
library(psych); library(psycho);

# Data visualization
library(ggplot2); library(forcats);

# Exporting data
library(writexl);


# Import Run Data ---------------------------------------------------------


