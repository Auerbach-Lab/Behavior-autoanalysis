
# Find an entry -----------------------------------------------------------
run_archive %>% rowid_to_column() %>% filter(date == "20230118" & rat_name == "BP3")


# Modify assignment -------------------------------------------------------
run_archive[88,]$assignment[[1]]$task = "Rxn"

