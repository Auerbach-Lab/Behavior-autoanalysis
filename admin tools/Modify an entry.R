
# Find an entry -----------------------------------------------------------
row_to_modify = run_archive %>% rowid_to_column() %>% filter(date == "20230119" & rat_name == "Blue2")
print(row_to_modify)
line_to_modify = row_to_modify$rowid

# Assignment change -------------------------------------------------------
run_archive[88,]$assignment[[1]]$task #= "Rxn"


# Weight change -----------------------------------------------------------
run_archive[line_to_modify,]$weight #= 535


# Save modification -------------------------------------------------------
save(run_archive, file = paste0(user_settings$projects_folder, "run_archive.Rdata"), ascii = TRUE, compress = FALSE)


