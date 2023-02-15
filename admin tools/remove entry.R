#TODO: make this work filter_arguments = 
bad_rats = c("GP5")
bad_date = "20221118"
# Restore assignment from the 'old' setting
restore = FALSE
# Backs up loseable data such as comments, weight, omit list
backup_data = FALSE


# Function ---------------------------------------------------------------_
clean_archives <- function(entry) {
  df = run_archive %>% filter(UUID == entry)
  writeLines(paste0("Cleaning ", df$rat_name, "'s entry on ", df$date, " ..."))
  
# Backup lose-able data
  if(backup_data) {
    key_data = df %>% select(all_of(c("date", "rat_ID", "rat_name", "weight", "omit_list", "comments"))) %>% 
      mutate(date_removed = Sys.Date() %>% as.character(),
             omit_list = as.character(omit_list))
    deleted_entries = read.csv(paste0(projects_folder, "deleted_entries.csv"))
    deleted_entries = bind_rows(deleted_entries, key_data)
    write.csv(deleted_entries, paste0(projects_folder, "deleted_entries.csv"), row.names = FALSE)
    writeLines("\tData backed up")
  } else writeLines("\tData NOT backed up")
  
# Wipe from trials archive:
  experiment = df$assignment %>% .[[1]] %>% pluck("experiment")
  variable_name = paste0(experiment, "_archive")
  filename = paste0(projects_folder, variable_name, ".csv.gz")
  # load archive & clean
  temp = fread(filename) %>%
    filter(UUID != df$UUID)
  fwrite(temp, file = filename)
  rm(temp)
  # save archive
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
                         "Data ", if_else(backup_data, "will", "will NOT"), " be saved and ", 
                         "previous assignment ", if_else(restore, "will", "will NOT"), " be restored"), 
            graphics = FALSE), 
       lapply(Bad_entries %>% .$UUID, clean_archives), writeLines("Stopped. Entries remain."))

rm(list = c("Bad_entries", "restore", "bad_rats", "bad_date"))

