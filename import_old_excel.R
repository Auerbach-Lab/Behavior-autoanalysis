require(openxlsx)
require(dplyr)
require(stringr)


names = c("Date", "Filename", "Weight", "Comments/Observations", "Experiment", "Phase", "Task", "Detail", "rat_name")
folder = "A:/Coding/Behavior-autoanalysis/Projects"

readExcel <- function(filename, cols = c(1:3, 23, 28:31)) {
  filename = paste(folder, filename, sep = "/")
  ratID_list = getSheetNames(filename) %>% stringr::str_subset(pattern = "[:digit:]$") %>% as.list() # Sheet names are rat names
  r = lapply(ratID_list, function(X) readWorkbook(filename, sheet = X, startRow = 3, cols = cols, colNames = TRUE, detectDates = TRUE) %>%
               dplyr::mutate(rat_name = stringr::str_replace_all(string = X, pattern = "[:space:]", replacement = ""))
  ) %>% dplyr::bind_rows() %>% dplyr::rename(Weight = X3) %>% setNames(names)

  return(r)
}

old_excel_archive = setNames(data.frame(matrix(ncol = 9, nrow = 0)), names) %>%
  rbind(readExcel("Fmr1-LE_Grp1_BP_LP_Purple.xlsx")) %>%
  rbind(readExcel("Noise_TTS_Gp1_Green-Orange.xlsx")) %>%
  rbind(readExcel("Tsc2_Eker_Gp1_RP_GP_TP.xlsx")) %>%
  rbind(readExcel("Oddball Pilot1 Blue1-4.xlsx", cols = c(1:3, 36, 48:51)))


rm(names, folder, readExcel)
