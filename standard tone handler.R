
# Build File Name ---------------------------------------------------------

go_kHz_range = paste0(file_summary %>% dplyr::filter(Type == 1) %>% .$`Freq (kHz)` %>% min(), "-",
                      file_summary %>% dplyr::filter(Type == 1) %>% .$`Freq (kHz)` %>% max(), "kHz")

go_dB_range = paste0(file_summary %>% dplyr::filter(Type == 1) %>% .$min %>% unique(), "-",
                     file_summary %>% dplyr::filter(Type == 1) %>% .$max %>% unique(), "dB")

has_Response_window = (response_window != 2)
has_TR = (trigger_senesitivity != 200)
has_BG = (background_type != "None")

BG = if (has_BG) paste0(stringr::str_remove(pattern = ".mat", string = background_file), "_", background_dB, "dB")

if (has_Response_window & has_TR & has_BG) {
  run_file_name =  paste0(go_kHz_range, "_", go_dB_range, "_", duration, "ms_", Lockout, "s_", response_window, "s_", "TR", trigger_senesitivity, "ms_", BG)
} else if (has_Response_window & has_TR ) {
  run_file_name =  paste0(go_kHz_range, "_", go_dB_range, "_", duration, "ms_", Lockout, "s_", response_window, "s_", "TR", trigger_senesitivity, "ms")
} else if (has_Response_window) {
  run_file_name =  paste0(go_kHz_range, "_", go_dB_range, "_", duration, "ms_", Lockout, "s_", response_window, "s")
} else if (has_TR ) {
  run_file_name =  paste0(go_kHz_range, "_", go_dB_range, "_", duration, "ms_", Lockout, "s_", "TR", trigger_senesitivity, "ms")
}

rm(list = c("background_file", "BG", "dB_step_size", "Delay", "has_BG", "has_Response_window", "Lockout", "no_go_max_touching", "nose_light", "trigger_senesitivity"))

# Check file name agreement -----------------------------------------------

# Check for mismatched file name
# The background check is a temporary measure for files predating the rename from BG_PKN.mat to BG_PNK.mat (occured 7/1/2022)
if (run_file_name != file_name) {
  comp = waldo::compare(run_file_name %>% str_split(pattern = ""),
                        file_name %>% str_split(pattern = ""),
                        x_arg = "Name from run",
                        y_arg = "Expected name")
  warning(paste("Missmatched file names.", comp, sep = "\n"))
  Warnings = append(Warnings, "Check: Missmatched file names.")
}

rm(list = c("run_file_name"))


# By stim Summary Data ----------------------------------------------------

# Get Hits/Misses/CRs/FAs and Hit %
source("~/GitHub/Behavior-autoanalysis/Hit_percent_by_stim.R")

# Add Attempts
source("~/GitHub/Behavior-autoanalysis/Attempts_by_stim.R")

# Add average reaction time
source("~/GitHub/Behavior-autoanalysis/RXN_by_stim.R")
