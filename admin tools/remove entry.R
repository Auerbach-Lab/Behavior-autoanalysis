#TODO: make this work filter_arguments = 

#check bad file
Bad_entry = run_archive %>% filter(date == "20230120" & rat_name == "Blue4")
print(Bad_entry)

# Check but doesn't actually do anything other than keep you from blindly continuing
menu(c("Yes", "No"), title=paste0("Do you want to DELETE the run from ", Bad_entry$date, " for ", Bad_entry$rat_name, "?"))

#Get UUID
UUID_to_remove = Bad_entry %>% .$UUID

#Wipe from trials archive:
experiment = Bad_entry %>% .$assignment %>% .[[1]] %>% pluck("experiment")
variable_name = paste0(experiment, "_archive")
filename = paste0(user_settings$projects_folder, variable_name, ".Rdata")

load(filename)
temp = get(variable_name) %>%
  filter(UUID != UUID_to_remove)

assign(variable_name, temp)
rm(temp)

save(list = get("variable_name"), file = filename, ascii = TRUE, compress = FALSE)
rm(list = get("variable_name"))


#Wipe UUID from run archive:
run_archive = filter(run_archive, UUID != UUID_to_remove)
save(run_archive, file = paste0(user_settings$projects_folder, "run_archive.Rdata"), ascii = TRUE, compress = FALSE)

rm(list = c("Bad_entry", "UUID_to_remove"))
InitializeMain()
