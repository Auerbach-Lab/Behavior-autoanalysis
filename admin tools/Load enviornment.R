InitializeMain <- function() {
  Load_Packages <- function() {
    # data loading external file formats
    library(R.matlab);
    
    # data manipulation
    library(tidyverse); library(dplyr); library(tidyr); library(rlang); library(stringr); library(purrr);
    
    # analysis & visualization
    library(psycho); library(ggplot2);
  }
  
  Load_Packages()
  options(warn = 1) # we want to display warnings as they occur, so that it's clear which file caused which warnings
  source("Z:/Behavior-autoanalysis/settings.R")  # hardcoded user variables
  
  rat_archive <<- read.csv(paste0(projects_folder, "rat_archive.csv"), na.strings = c("N/A","NA"))
  load(paste0(projects_folder, "run_archive.Rdata"), .GlobalEnv)
}


# MAIN ---------------------------------------------------------
InitializeMain()
