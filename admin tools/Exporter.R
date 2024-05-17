# NOTE:
# This script is NOT fully automated.
# You must manually handle aligning the column names in Flatten since they vary by experiment 

# User selections ---------------------------------------------------------

# You can only select one experiment to keep because each has different
# statistics and so can't be easily combined
experiments_to_keep = "Fmr1 SD"

# What columns you want included in the final output
# NOT: TH, dprime, FA_detailed, reaction and hit_detailed (once added)
columns_to_keep_unnested = c("date", "rat_name", "rat_ID",
                             "DOB", "Sex", "Genotype", "HL_date",
                             "file_name", "experiment", "phase", "task", "detail",
                             "stim_type", "analysis_type", "complete_block_count", 
                             "trial_count", "hit_percent", "FA_percent", "UUID")

# This is only TH, dprime, FA_detailed, reaction and hit_detailed (once added)
columns_to_keep_nested = c("dprime", "reaction")

# Do not include records before the following date
# Date format should be: YYYYMMDD
# for all records, set to NULL
start_date_of_desired_records = NULL
end_date_of_desired_records = NULL


save_location = "C:/Users/Noelle/Box/Behavior Lab/Shared/Walker/"

save_file_name = glue("{str_flatten(experiments_to_keep, collapse = '+')}_data_exported")

# Load Data ---------------------------------------------------------------
load(glue("{projects_folder}/run_archive.Rdata"), .GlobalEnv)

rat_decoder = fread(glue("{projects_folder}/rat_archive.csv"),
                    select = c("Rat_ID", "DOB", "Sex", "Genotype", "HL_date"))


# Clear old data if exists ------------------------------------------------
rm(dataset, decoded_data, selected_data, data_for_export, data1, data2)


# Remove bad data ---------------------------------------------------------
#TODO: deal with multiple runs in a day

dataset = run_archive %>%
  # Omit Invalid runs
  filter(invalid != "TRUE") %>%
  #Omit runs with wrong delay window, the negate means it returns non-matches
  #ISSUE: gives warning because it expects a vector not a dataframe
  filter(str_detect(warnings_list, pattern = "wrong delay window", negate = TRUE))


# Filter to date range ----------------------------------------------------
if(! is.null(start_date_of_desired_records)) dataset = filter(run_archive, date >= start_date_of_desired_records)
if(! is.null(start_date_of_desired_records)) dataset = filter(run_archive, date <= end_date_of_desired_records)


# Decode rat info ---------------------------------------------------------
decoded_data = dataset %>%
  # decode rats
  left_join(rat_decoder, by = c("rat_ID" = "Rat_ID"))


# Filter down to desired data ---------------------------------------------
selected_data = decoded_data %>%
  # Get essential columns in usable form; expands the dataframe
  unnest_wider(assignment) %>% 
  # if multiple types of experiments, this may cause issues if not pre-filtered
  unnest_wider(stats) %>%
  select(all_of(union(columns_to_keep_unnested, columns_to_keep_nested))) %>%
  # drop Oddball and Octave
  filter(experiment %in% experiments_to_keep)


# Flatten and join correctly ----------------------------------------------
# TODO: automate this with a loop script
# This mainly applies to TH, FA_detailed, reaction and hit_detailed (once added)
# note, this is both faster and allows for proper joining so that some columns aren't incorrectly duplicated

count = 0
dataframes_to_merge = c()

for (i in columns_to_keep_nested) {
  count <- (count + 1)
  name = glue("data{count}")
  original_columns = names(selected_data)
  tempdf = selected_data %>% 
    select(!!!columns_to_keep_unnested, i) %>%
    unnest(i)
  new_columns = names(tempdf)
  print(glue("Unnested {i} in {name}\n\\     New columns: {str_flatten_comma(setdiff(new_columns, original_columns))}"))
  
  assign(name, tempdf)
  dataframes_to_merge = c(dataframes_to_merge, name)
}

writeLines("\nManually align the new columns so that the final data table can be finished\n\n")
stop("Halted for manual input")

## Manual start: Merge dataframes ------ 
# MUST BE DONE MANUALLY WITH THE NEW COLUMNS in the console if not already clearly specified below
data_for_export = left_join(data1, data2, 
                            by = join_by(!!!columns_to_keep_unnested, # list of normal columns
                                         # shared columns with different names
                                         
                                         ### For Rxn & FA_detailed (Oddball) ----
                                         # position == `Inten (dB)`)
                                         
                                         ### For Reaction & dprime ----
                                         dB == `Inten (dB)`, Freq == `Freq (kHz)`, Dur == `Dur (ms)`)
                            
                            )


# Create any new columns needed -------------------------------------------

data_for_export = data_for_export %>%
  # make columns for line and genotype
  mutate(line = str_extract(Genotype, pattern = "^.*(?=(-|_)LE)"),
         genotype = str_extract(Genotype, pattern = "(?<=LE_).*$"),
         line = if_else(is.na(line), "SD", line),
         genotype = if_else(is.na(genotype), "WT", genotype))


# Export ------------------------------------------------------------------
data_for_export %>%
  fwrite(glue("{save_location}{save_file_name}_", str_remove_all(Sys.Date(), "-"),".csv"), row.names = FALSE)


# OPTIONAL Trial Exporter -------------------------------------------------

trial_archive = fread(paste0(projects_folder, experiments_to_keep, "_archive.csv.gz"))
trial_archive_for_export = filter(trial_archive, UUID %in% unique(data_for_export$UUID))

## Export ----------
trial_archive_for_export %>%
  fwrite(glue("{save_location}{save_file_name}_trials_", str_remove_all(Sys.Date(), "-"),".csv"), row.names = FALSE)
