#TODO: make this work filter_arguments = 

#Get current run_achive
InitializeMain()

#Test or check files to be removed
run_archive %>% filter(date == "20230130" & rat_name %in% c("Purple3", "Purple4")) %>% select(date, rat_name, weight, comments) #%>% write.csv(paste0(projects_folder, "to be fixed or removed.csv"), row.names = FALSE)
#TODO lapply with the following being a function triggered by the entry of:
# Check but doesn't actually do anything other than keep you from blindly continuing
switch(menu(c("Yes", "No"), title=paste0("Do you want to DELETE the run from ", Bad_entry$date, " for ", Bad_entry$rat_name, "?"), graphics = FALSE), print("Yes"), print("No"))

#check bad file
Bad_entry = run_archive %>% filter(date == "20230130" & rat_name == "Purple3")
print(Bad_entry)

# Check but doesn't actually do anything other than keep you from blindly continuing
# menu(c("Yes", "No"), title=paste0("Do you want to DELETE the run from ", Bad_entry$date, " for ", Bad_entry$rat_name, "?"))

#Get UUID
UUID_to_remove = Bad_entry %>% .$UUID

#Wipe from trials archive:
experiment = Bad_entry %>% .$assignment %>% .[[1]] %>% pluck("experiment")
variable_name = paste0(experiment, "_archive")
filename = paste0(projects_folder, variable_name, ".Rdata")

load(filename)
temp = get(variable_name) %>%
  filter(UUID != UUID_to_remove)

assign(variable_name, temp)
rm(temp)

save(list = get("variable_name"), file = filename, ascii = TRUE, compress = FALSE)
rm(list = get("variable_name"))


#Wipe UUID from run archive:
run_archive = filter(run_archive, UUID != UUID_to_remove)
save(run_archive, file = paste0(projects_folder, "run_archive.Rdata"), ascii = TRUE, compress = FALSE)


# Restore assignment ------------------------------------------------------
rat_archive[rat_archive$Rat_name == Bad_entry$rat_name,]$Assigned_Filename <- rat_archive[rat_archive$Rat_name == Bad_entry$rat_name,]$Old_Assigned_Filename
rat_archive[rat_archive$Rat_name == Bad_entry$rat_name,]$Assigned_Experiment <- rat_archive[rat_archive$Rat_name == Bad_entry$rat_name,]$Old_Assigned_Experiment
rat_archive[rat_archive$Rat_name == Bad_entry$rat_name,]$Assigned_Phase <- rat_archive[rat_archive$Rat_name == Bad_entry$rat_name,]$Old_Assigned_Phase
rat_archive[rat_archive$Rat_name == Bad_entry$rat_name,]$Assigned_Task <- rat_archive[rat_archive$Rat_name == Bad_entry$rat_name,]$Old_Assigned_Task
rat_archive[rat_archive$Rat_name == Bad_entry$rat_name,]$Assigned_Detail <- rat_archive[rat_archive$Rat_name == Bad_entry$rat_name,]$Old_Assigned_Detail
write.csv(rat_archive, paste0(projects_folder, "rat_archive.csv"), row.names = FALSE)


rm(list = c("Bad_entry", "UUID_to_remove"))
InitializeMain()

