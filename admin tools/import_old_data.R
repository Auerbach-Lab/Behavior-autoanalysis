# For loading old files:

projects_folder = "Z:/Behavior-autoanalysis/"
source("Y:/GitHub/Behavior-autoanalysis/main.R")

# Get files
load(paste0(projects_folder, "old_excel_archive.Rdata"))
directory = "C:/Users/Noelle/Box/Behavior Lab/Projects (Behavior)/Archive"  # slashes must be either / or \\
files = list.files(directory, pattern = "\\.mat$", recursive = TRUE)
# files = files[str_which(files, pattern = "^(?!.*(Archive|Oddball|Gap|Tsc|Fmr|TTS))")] # Drop un-annotated files

# Select only project we want to add
# # Oddball stopped at 10/2022
# files = files[str_which(files, pattern = "Oddball")] # Drop un-annotated files
files = files[str_which(files, pattern = "Fmr1")] # Drop un-annotated files

# For Tsc2-LE
# files = files[str_which(files, pattern = ".*Tsc2.*/data/.*/(?!((Blue|Red).*))")] # Exclude Blue Pilot Group as they are all WT
# For TTS:
# files = files[str_which(files, pattern = ".*TTS.*/data/.*/(?!((Orange6).*))")] # Exclude Orange 6 as she is dropped

# files = files[str_which(files, pattern = ".*/data/(?!(2022060(9|7)/RP1.*))")] # Bad second file for RP1 on 6/9/22 - dprime giving error but transiently
# files = files[str_which(files, pattern = ".*/data/(?!(20221128/GP3.*))")] # Bad second file
# files = files[str_which(files, pattern = ".*/data/(?!(20220829/BP3.*))")] # Bad second file
# files = files[str_which(files, pattern = ".*/data/(?!(20220706))")] # Bad data on 7/6/22 - no creation date.
# files = files[str_which(files, pattern = ".*/data/(?!(20220615/Green2_Green2_4-32kHz_MIX5step_50ms_16s_1s_BG_PNK_50dB_20220615-105130_BOX#006.mat*))")] # 1 trial causing issues

# Select Month to load
files = files[str_which(files, pattern = ".*/data/Group 1/202204")]


files = paste0(directory, "/", files)
writeLines(paste0("Loading ", length(files), " files.\n\n"))

# run main
lapply(files, Process_File, old_file = TRUE, ignore_name_check = TRUE, exclude_trials = "")
writeLines(paste0("Done with back date loading. ", length(files), " files added."))
writeLines(paste0("\n\nDONE with back date loading. ", length(files), " files added."))
