#TODO: make this work filter_arguments = 
bad_rats = c("GP6")
bad_date = "20230203"
restore = TRUE


# Functions ---------------------------------------------------------------
clean_archives <- function(entry) {
  df = run_archive %>% filter(UUID == entry)
  writeLines(paste0("Cleaning ", df$rat_name, "'s entry on ", df$date, " ..."))
  
# Backup loseable data
  key_data = df %>% select(all_of(c("date", "rat_ID", "rat_name", "weight", "comments"))) %>% mutate(date_removed = Sys.Date())
  deleted_entries = read.csv(paste0(projects_folder, "deleted_entries.csv"))
  deleted_entries = bind_rows(deleted_entries, key_data)
  write.csv(deleted_entries, paste0(projects_folder, "deleted_entries.csv"), row.names = FALSE)
  writeLines("\tData backed up")
  
# Wipe from trials archive:
  experiment = df$assignment %>% .[[1]] %>% pluck("experiment")
  variable_name = paste0(experiment, "_archive")
  filename = paste0(projects_folder, variable_name, ".Rdata")
  # load archive & clean
  load(filename)
  temp = get(variable_name) %>%
    filter(UUID != df$UUID)
  assign(variable_name, temp)
  rm(temp)
  # save archive
  save(list = get("variable_name"), file = filename, ascii = TRUE, compress = FALSE)
  writeLines(paste0("\tTrials removed from ", variable_name))
  
# Wipe UUID from run archive
  run_archive = filter(run_archive, UUID != df$UUID)
  save(run_archive, file = paste0(projects_folder, "run_archive.Rdata"), ascii = TRUE, compress = FALSE)
  writeLines("\tRun archive cleaned")
  
# Restore assignment
  if(restore) {
    rat_archive[rat_archive$Rat_name == df$rat_name,]$Assigned_Filename <- rat_archive[rat_archive$Rat_name == df$rat_name,]$Old_Assigned_Filename
    rat_archive[rat_archive$Rat_name == df$rat_name,]$Assigned_Experiment <- rat_archive[rat_archive$Rat_name == df$rat_name,]$Old_Assigned_Experiment
    rat_archive[rat_archive$Rat_name == df$rat_name,]$Assigned_Phase <- rat_archive[rat_archive$Rat_name == df$rat_name,]$Old_Assigned_Phase
    rat_archive[rat_archive$Rat_name == df$rat_name,]$Assigned_Task <- rat_archive[rat_archive$Rat_name == df$rat_name,]$Old_Assigned_Task
    rat_archive[rat_archive$Rat_name == df$rat_name,]$Assigned_Detail <- rat_archive[rat_archive$Rat_name == df$rat_name,]$Old_Assigned_Detail
    write.csv(rat_archive, paste0(projects_folder, "rat_archive.csv"), row.names = FALSE)
    writeLines("\tAssignment restored")
  } else {writeLines("\tRat assignment untouched")}
  
  writeLines("Done.")
}


# Main --------------------------------------------------------------------
#Get current run_achive
InitializeMain()

#Test or check files to be removed
Bad_entries = run_archive %>% filter(date == bad_date & rat_name %in% bad_rats)
print(select(Bad_entries, all_of(c("date", "rat_name"))))

switch(menu(c("Yes", "No"), title=paste0("Do you want to DELETE the runs from run_archive?"), graphics = FALSE), lapply(Bad_entries %>% .$UUID, clean_archives), writeLines("Stopped. Entries remain."))

rm(list = c("Bad_entries", "restore", "bad_rats", "bad_date"))
InitializeMain()

