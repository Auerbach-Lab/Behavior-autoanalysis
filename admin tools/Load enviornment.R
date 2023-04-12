InitializeMain <- function() {
  Load_Packages <- function() {
    # data loading external file formats
    library(R.matlab); library(data.table); library(openxlsx); library(xml2); library(zip); library(data.table);
    
    # data manipulation
    library(tidyverse); library(dplyr); library(tidyr); library(rlang); library(stringr); library(purrr); library(lubridate); library(glue)
    
    # analysis & visualization
    library(psycho); library(ggplot2); library(hrbrthemes); library(shiny);
  }
  
  Load_Packages()
  options(warn = 1) # we want to display warnings as they occur, so that it's clear which file caused which warnings
  source(paste0(projects_folder, "settings.R"))  # hardcoded user variables
  
  rat_archive <<- read.csv(paste0(projects_folder, "rat_archive.csv"), na.strings = c("N/A","NA"))
  load(paste0(projects_folder, "run_archive.Rdata"), .GlobalEnv)
}


# MAIN ---------------------------------------------------------
projects_folder = "Z:/Behavior-autoanalysis/"
InitializeMain()
