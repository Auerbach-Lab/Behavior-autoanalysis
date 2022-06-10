
# Loading current file ----------------------------------------------------
current_file = readMat("C:/Users/Noelle/Box/Behavior Lab/Projects (Behavior)/TTS/data/20220609/Green1_4-32kHz_30-90dB_50ms_8s_1s_TR100ms_20220609-101834_BOX#001.mat")

# File Breakdown ----------------------------------------------------------

# Table of reaction times with # of stim
# TODO:
# - decode stim to frequency (kHz), loudness (dB), Duration (ms)
# -
current_file$result

# File 'header' data
header = current_file[["stim"]]
# make elements callable by name because somehow names are coming in as row.names
names(header) = row.names(header)

# Get file name -----------------------------------------------------------

# works
actual_file_name = current_file[["stim"]][[1]]

# Should work and is less hard coded but doesn't
# this is because the
current_file[["stim"]][["name"]]
