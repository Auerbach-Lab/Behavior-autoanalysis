bad_rats = c("Blue6")
# "today" or a specific date in YYYYMMDD format
bad_date = 20240513
# Restore assignment from the 'old' setting
restore = FALSE

source(paste0(projects_folder, "/admin tools/clean archives.R"))

# Function ---------------------------------------------------------------_
clean_wrapper <- function(entry, date) {
  clean_archives(entry, date)

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
  return(glue("Deleted entry {entry}"))
}


# Main --------------------------------------------------------------------
#Get current run_achive
InitializeMain()

# Test or check files to be removed
bad_date = ifelse(bad_date == "today", str_remove_all(Sys.Date(), "-"), bad_date)
Bad_entries = run_archive %>% filter(date == bad_date & rat_name %in% bad_rats)

# Clean NA file
# NA_archive = fread(paste0(projects_folder, "_archive.csv.gz"))
# Bad_UUIDs = NA_archive$UUID %>% unique()
# Bad_entries = run_archive %>% filter(UUID %in% Bad_UUIDs)
# writeLines(paste(length(Bad_UUIDs), "entries in the NA_archive"))

# Show entries to be cleaned
print(select(Bad_entries, all_of(c("date", "rat_name"))))

switch(menu(c("Yes", "No"),
            title=paste0("Do you want to DELETE the runs from run_archive?\n Note: ",
                         "Data WILL be saved and ",
                         "previous assignment ", if_else(restore, "WILL", "will NOT"), " be restored"),
            graphics = FALSE),
       # 1 (Yes): Write file
        lapply(Bad_entries %>% .$UUID, clean_wrapper),
       # 2 (No): Abort
        writeLines("Stopped. Entries remain."))

InitializeMain()
# check they are gone
filter(run_archive, UUID %in% Bad_entries$UUID) %>% print

rm(list = c("Bad_entries", "restore", "bad_rats", "bad_date"))

