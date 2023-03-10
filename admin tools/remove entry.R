bad_rats = c("BP1")
bad_date = "20230223"
# Restore assignment from the 'old' setting
restore = TRUE
# Backs up loseable data such as comments, weight, omit list
backup_data = TRUE


# Function ---------------------------------------------------------------_
clean_archives <- function(entry) {
  df = run_archive %>% filter(UUID == entry)
  writeLines(paste0("Cleaning ", df$rat_name, "'s entry on ", df$date, " ..."))
  
# Backup lose-able data
  if(backup_data) {
    # Get data to save
    key_data = df  %>% select(all_of(c("date", "rat_ID", "rat_name", "weight", "omit_list", "comments", "assignment") %>% unnest(assignment))) %>% 
      mutate(date_removed = Sys.Date() %>% as.character(),
             omit_list = as.numeric(omit_list))
    # append to running CSV
    fwrite(key_data, file = paste0(projects_folder, "deleted_entries.csv"), append = file.exists(paste0(projects_folder, "deleted_entries.csv")))
    writeLines("\tData backed up")
  } else writeLines("\tData NOT backed up")
  
# Wipe from trials archive:
  experiment = df$assignment %>% .[[1]] %>% pluck("experiment")
  variable_name = paste0(experiment, "_archive")
  filename = paste0(projects_folder, variable_name, ".csv.gz")
  # load archive
  trial_archive = fread(filename)
  # clean archive
  temp = filter(trial_archive, UUID != df$UUID)
  # save cleaned archive
  fwrite(temp, file = filename)
  writeLines(paste0("\tTrials removed from ", variable_name))
  
# Wipe UUID from run archive
  run_archive = filter(run_archive, UUID != df$UUID)
  save(run_archive, file = paste0(projects_folder, "run_archive.Rdata"), ascii = FALSE, compress = FALSE)
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
  InitializeMain()
}


# Main --------------------------------------------------------------------
#Get current run_achive
InitializeMain()

#Test or check files to be removed
Bad_entries = run_archive %>% filter(date == bad_date & rat_name %in% bad_rats)
print(select(Bad_entries, all_of(c("date", "rat_name"))))

switch(menu(c("Yes", "No"), 
            title=paste0("Do you want to DELETE the runs from run_archive?\n Note: ", 
                         "Data ", if_else(backup_data, "WILL", "will NOT"), " be saved and ", 
                         "previous assignment ", if_else(restore, "WILL", "will NOT"), " be restored"), 
            graphics = FALSE),
       # 1 (Yes): Write file
        lapply(Bad_entries %>% .$UUID, clean_archives), 
       # 2 (No): Abort
        writeLines("Stopped. Entries remain."))

rm(list = c("Bad_entries", "restore", "bad_rats", "bad_date"))

