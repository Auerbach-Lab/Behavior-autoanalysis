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
  
  print(as_tibble(r))

  return(r)
}

writeLines("Creating old archives")
cat("\tAdding: ")
old_excel_archive = setNames(data.frame(matrix(ncol = 10, nrow = 0)), names)
cat("TTS...")
old_excel_archive = rbind(old_excel_archive, readExcel("TTS_Gp1_Green-Orange.xlsx"))
cat("Fmr1-LE...")
old_excel_archive = rbind(old_excel_archive, readExcel("Fmr1-LE_Grp1_BP_LP_Purple.xlsx"))
cat("Fmr1 SD...")
old_excel_archive = rbind(old_excel_archive, readExcel("Fmr1_SD_Gp2_RedTealLime.xlsx", cols = c(1:3, 23, 27:32)))
cat("Tsc2-LE...")
old_excel_archive = rbind(old_excel_archive, readExcel("Tsc2_Eker_Gp1_RP_GP_TP.xlsx"))
cat("Octave (Fmr1-LE pilot)...") 
old_excel_archive = rbind(old_excel_archive, readExcel("Fmr1_SD_Gp1_Purple - Tone Discrimination.xlsx", cols = c(1:3, 23, 28:33)))
cat("Octave (Fmr1 SD pilot)...") 
old_excel_archive = rbind(old_excel_archive, readExcel("Fmr1_SD_Gp2_RedTealLime - Tone Discrimination.xlsx", cols = c(1:3, 24, 28:33)))
#   cat("Gap Detection...")  
# old_excel_archive = rbind(old_excel_archive, readExcel("GD_Grp1_Red_Teal.xlsx"))
  cat("Oddball (LE pilot)")  
old_excel_archive = rbind(old_excel_archive, readExcel("Oddball Pilot1 Blue1-4.xlsx", cols = c(1:3, 36, 48:52)))

writeLines("\nDONE")

old_excel_archive = old_excel_archive %>%
  dplyr::mutate(Invalid = dplyr::if_else(! is.na(Invalid), "TRUE", "", missing = "")) %>%
  filter(! Filename %in% c("Did not run", "Handling")) %>%
  filter(Date < depricated_date)

save(old_excel_archive, file = paste0(projects_folder, "old_excel_archive.Rdata"), ascii = TRUE, compress = FALSE)

rm(list = c("names", "folder", "readExcel", "depricated_date"))
