library(openxlsx)
library(dplyr)
library(stringr)


names = c("Date", "Filename", "Weight", "Comments/Observations", "Experiment", "Phase", "Task", "Detail", "Invalid", "rat_name")
folder = "Z:/Project Excels"
depricated_date = "2023-01-17"

readExcel <- function(filename, cols = c(1:3, 23, 28:32)) {
  filename = paste(folder, filename, sep = "/")
  ratID_list = getSheetNames(filename) %>% stringr::str_subset(pattern = "[:digit:]$") %>% as.list() # Sheet names are rat names
  r = lapply(ratID_list, function(X) readWorkbook(filename, sheet = X, startRow = 3, cols = cols, colNames = TRUE, detectDates = TRUE) %>%
               dplyr::mutate(rat_name = stringr::str_replace_all(string = X, pattern = "[:space:]", replacement = ""),
                             Invalid = as.character(Invalid))
  ) %>% dplyr::bind_rows() %>% dplyr::rename(Weight = X3) %>% setNames(names)

  return(r)
}

old_excel_archive = setNames(data.frame(matrix(ncol = 10, nrow = 0)), names) %>%
  # rbind(readExcel("Noise_TTS_Gp1_Green-Orange.xlsx")) %>%
  rbind(readExcel("Fmr1-LE_Grp1_BP_LP_Purple.xlsx")) %>%
  rbind(readExcel("Tsc2_Eker_Gp1_RP_GP_TP.xlsx")) %>%
  rbind(readExcel("GD_Grp1_Red_Teal.xlsx"))
  # rbind(readExcel("Oddball Pilot1 Blue1-4.xlsx", cols = c(1:3, 36, 48:52)))

old_excel_archive = old_excel_archive %>%
  dplyr::mutate(Invalid = dplyr::if_else(! is.na(Invalid), "TRUE", "", missing = "")) %>%
  filter(! Filename %in% c("Did not run", "Handling")) %>%
  filter(Date < depricated_date)

save(old_excel_archive, file = paste0(projects_folder, "old_excel_archive.Rdata"), ascii = TRUE, compress = FALSE)

rm(list = c("names", "folder", "readExcel", "depricated_date"))
