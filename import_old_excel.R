require(openxlsx)
require(dplyr)
require(stringr)

MainFolder = 'A:/Coding/Behavior-autoanalysis/Fake Project Folder'
setwd(MainFolder)

filename = "Noise_TTS_Gp1_Green-Orange.xlsx"

# Sheet names are rat names
# Load TTS spreadsheet (hard coded)
#TTS_RatID_list <- excel_sheets(filename) %>% .[1:11] %>% as.list()

ratID_list <- getSheetNames(filename) %>% stringr::str_subset(pattern = "[:digit:]$") %>% as.list()

old_excel_archive = lapply(ratID_list, function(X) readWorkbook(filename, sheet = X, startRow = 3, cols = c(1:3, 23, 27:30), colNames = TRUE, detectDates = TRUE) %>%
              dplyr::mutate(rat_name = stringr::str_replace_all(string = X, pattern = "[:space:]", replacement = ""))
     ) %>% dplyr::bind_rows() %>% dplyr::rename(Weight = X3)

rm(MainFolder, filename, ratID_list)
