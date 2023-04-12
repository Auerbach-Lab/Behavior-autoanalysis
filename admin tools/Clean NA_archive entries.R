InitializeMain()
load(paste0(projects_folder, "NA_archive.csv.gz"))


Bad_UUIDs = NA_archive %>% .$UUID %>% unique()
writeLines(paste(length(Bad_UUIDs), "entries in the NA_archive"))


Bad_entries = filter(run_archive, UUID %in% Bad_UUIDs)

clean_run_archive <- function() {
  writeLines("Cleaning...")
  run_archive = filter(run_archive, ! UUID %in% Bad_UUIDs)
  save(run_archive, file = paste0(projects_folder, "run_archive.Rdata"), ascii = TRUE, compress = FALSE)
  writeLines("Done.")
}

print(select(Bad_entries, all_of(c("date", "rat_name"))))

switch(menu(c("Yes", "No"), title=paste0("Do you want to DELETE the runs from run_archive?"), graphics = FALSE), clean_run_archive(), writeLines("Stopped. Entries remain."))

InitializeMain()
filter(run_archive, UUID %in% Bad_UUIDs) %>% print

rm(list = c("Bad_entries", "Bad_UUIDs"))

