test = run_archive_old[1:3,]
# Reopening and every matlab file would be very slow and undesireable.
# Therefore, I would like to just call the nested function of
# Calculate_Summary_Statistics on the archived trial data, but this doesn't
# appear to be possible. The next best thing is copying a bunch of that function
# and recreating it which is bad for the obvious reasons.
source(file = "main.R")

# find all archive files
archive_files = list.files(path = projects_folder, pattern = "^.*_archive.csv.gz$") %>% paste0(projects_folder, .) %>% data.frame(file = .) %>% 
  #TODO this must be updated to match the last letter of the previous folder: probably ('(?<=s/).*(?=_a)')
  # but for testing purposes its g/
  mutate(file_name = stringr::str_extract(file, pattern = '(?<=g/).*(?=.c)'),
         experiment = stringr::str_extract(file, pattern = '(?<=g/).*(?=_a)')) 

# load archive files:
loader <- function(df) {
  # name = str_remove(df["experiment"], pattern = "[:punct:]|[:space:]")
  # print(paste(name, " = fread:", df["file"]))
  assign(df["file_name"], fread(df["file"]), envir = .GlobalEnv)
  # return(paste(df["file_name"], " Done"))
}
# 1 indictes to iterate by rows
# archive = apply(archive_files, 1, loader, simplify = FALSE) %>% bind_rows()


ungrouping <- function(df, UUID, analysis_type) {
    Calculate_Stats_Detail <- function() {
      # stats_detailed Workflow -----------------
      print(UUID_of_run)
      trial_data = filter(archive, UUID == UUID_of_run)
      
      if(analysis$type %in% c("Oddball (Uneven Odds & Catch)", "Oddball (Uneven Odds)", "Oddball (Catch)", "Oddball (Standard)")) {
        stats_detail = Calculate_FA_Detailed_Oddball()
      }
      
      
      return("good")
    }
  
  
  # Workflow ---------------------------
  # print(UUID)
  # print(df)
  df$threshold = ungroup(df$threshold)
  df$reaction = ungroup(df$reaction)
  df$FA_detailed = NULL #drop column
  
  # load and calaculate stats_detailed
  # df$stats_detail = "good"
  df$stats_detail = Calculate_Stats_Detail(UUID_of_run = UUID)

  return(df)
}

test_modified = test %>% rowwise() %>% mutate(stats = list(ungrouping(stats, UUID = UUID, analysis_type = analysis_type)))
