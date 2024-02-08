source(paste0(projects_folder, "graphing unrolled.R"))

# Variables ---------------------------------------------------------------

Rat_Name = "Blue 2"
# Enter if you know otherwise set to NULL
Rat_ID = NULL

# Get Rat ID from name ----------------------------------------------------

# Clean rat name
Rat_Name = str_remove_all(Rat_Name, pattern = "[:space:]") %>% str_to_title()

if (is.null(Rat_ID)) {
  rat = rat_archive %>% 
    filter(is.na(end_date) & start_date < str_remove_all(Sys.Date(), "-")) %>%
    filter(Rat_name == Rat_Name) %>%
    select(all_of(Rat_name, Rat_ID, Box, Genotype, Sex, Old_Assigned_Experiment)) %>%
    print
  
  Rat_ID = rat$Rat_ID
}


graph = suppressMessages(Generate_Graph(rat_name = Rat_Name, ratID = Rat_ID))

print(graph$dprime_graph)
print(graph$rxn_graph)
