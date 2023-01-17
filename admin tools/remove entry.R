#TODO: make this work filter_arguments = 

#check bad file
Test = run_archive %>% filter(date == "20230117" & rat_name == "Orange12")
print(Test)

#Get UUID
UUID_to_remove = Test %>% .$UUID

#Wipe from trials archive:
experiment = Test %>% .$assignment %>% .[[1]] %>% pluck("experiment")
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
