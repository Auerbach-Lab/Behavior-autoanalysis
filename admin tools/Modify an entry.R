InitializeMain()

# Find an entry -----------------------------------------------------------
row_to_modify = run_archive %>% rowid_to_column() %>% filter(date == "20230209" & rat_name == "Purple4")
print(row_to_modify)
line_to_modify = row_to_modify$rowid


# Invalid Run -------------------------------------------------------------
# for marking a run as invalid due to circumstances
# anything marked as invalid should NOT be used for any follow up analysis
run_archive[line_to_modify,]$invalid #= "TRUE"

# Assignment change -------------------------------------------------------
run_archive[line_to_modify,]$assignment[[1]]$phase #= "Tone"
run_archive[line_to_modify,]$assignment[[1]]$task #= "Rxn"
run_archive[line_to_modify,]$assignment[[1]]$detail #= "Mixed"


# Weight change -----------------------------------------------------------
run_archive[line_to_modify,]$weight #= 239


# Warnings list modification ----------------------------------------------
run_archive[line_to_modify,]$warnings_list[[1]] #= run_archive[line_to_modify,]$warnings_list[[1]][-1]


# Save modification -------------------------------------------------------
save(run_archive, file = paste0(projects_folder, "run_archive.Rdata"), ascii = TRUE, compress = FALSE)

rm(list = c("row_to_modify", "line_to_modify"))
InitializeMain()

