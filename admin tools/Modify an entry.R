InitializeMain()

# Find an entry -----------------------------------------------------------
row_to_modify = run_archive %>% rowid_to_column() %>% filter(date == "20220122" & rat_name == "Orange4")
row_to_modify %>% select(date, time, rat_name, rat_ID, weight, scientist, rxnProblem, weightProblem, file_name, comments)
# row_to_modify %>% unnest_wider(stats) %>% select(rowid, date, rat_name, rat_ID, weight, trial_count, scientist, rxnProblem, weightProblem, file_name, comments)
line_to_modify = row_to_modify$rowid


# Invalid Run -------------------------------------------------------------
# for marking a run as invalid due to circumstances
# anything marked as invalid should NOT be used for any follow up analysis
run_archive[line_to_modify,]$invalid #= "TRUE"

# Assignment change -------------------------------------------------------
run_archive[line_to_modify,] %>% select(date, rat_name, rat_ID, file_name, assignment) %>% unnest_wider(assignment) %>% select(-comment)
run_archive[line_to_modify,]$assignment[[1]]$phase #= "Tones"
run_archive[line_to_modify,]$assignment[[1]]$task #= "Rxn"
run_archive[line_to_modify,]$assignment[[1]]$detail #= "Mixed"


# Weight change -----------------------------------------------------------
run_archive[line_to_modify,]$weight #= 239


# Warnings list modification ----------------------------------------------
run_archive[line_to_modify,]$warnings_list[[1]] #= run_archive[line_to_modify,]$warnings_list[[1]][-1]


# Feeding or Rxn notes modification ---------------------------------------
run_archive[line_to_modify,] %>% select(date, rat_name, rat_ID, file_name, scientist, rxnProblem, weightProblem)
run_archive[line_to_modify,]$weightProblem = paste0(run_archive[line_to_modify,]$weightProblem,
                                                    "NJ: Weight dropping so gave an extra male pellet")


# Save modification -------------------------------------------------------
save(run_archive, file = paste0(projects_folder, "run_archive.Rdata"), ascii = TRUE, compress = FALSE)

rm(list = c("row_to_modify", "line_to_modify"))
InitializeMain()

