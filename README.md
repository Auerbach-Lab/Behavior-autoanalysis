# PiedPiper 1.0.1

## Installation
Download the repository and, as necessary, update installed R packages from the dependency list below. Run the `font setup.R` admin tool line by line.

### Dependencies
	tidyverse >= 2.0.0
	data.table >= 1.14.6
	ggplot2 >= 3.4.0
	glue >= 1.6.2
	openxlsx >= 4.2.5.1
	psycho >= 0.6.1
	R.matlab >= 3.7.0
	zip >= 2.2.2
	
The versions of many of these packages matter, particularly openxlsx and the tidyverse packages.
Be sure to update to at least the listed versions of the dependencies above.

## Usage
Throughout each day, persons responsible for running the experimental boxes (probably undergraduates) will run `app.R`, filling out the form for an individual rat's run and using the file picker (the Browse... button) to select the corresponding `.mat` file, then press Analyze. The user will be asked to examine two graphs (and possibly more, if there are any concerns raised by the initial two) to verify that the rat's weight and trial count trends are acceptable, that the data looks accurate and consistent with this rat's prior performance, etc. Once there is high confidence that the data is good and belongs to the named rat, the user will choose 'Save Run' at which point the results will be added to the archives on disk.

At the end of the day, the person responsible for making the assingments to be used the next day will run `supervisor-summarize.R`. It will create and open a summary .xlsx (Excel) file that displays the data and statistics for each rat that are used in choosing assignments. The supervisor will use the drop-down menus and the filename entry field to specify the configuration files that each rat should next be run on. The supervisor can also leave a note on any rat that will remain until it is cleared, intended for e.g. planning ahead.

## Maintenance
### New Experiment
Adding a new experiment requires, at minimum, updating `experiment_details.csv` to add the new experiment and denote its corresponding phases. If the experiment uses existing phase types, no further work is required.

If new phase types are added, `main.R` and `supervisor-summarize.R` must be updated to correctly process them. Non-exhaustively, in main, `Identify_Analysis_Type()` and `Build_Filename()` will need new definitions, and it is possible that `Calculate_Summary_Statistics()` will also. In supervisor-summarize, `Build_Counts()` and the task-specific and phase-specific portions of `Build_Table()` will need updating. In addition, the phase's tasks and details will have to be listed in `experiment_details.csv`.

### New Rat
The new rat should be added to the `rat_archive.csv` file, either by opening it with a spreadsheet program or through R.

### Hearing Loss, Rat Retirement/Death, etc
The `rat_archive.csv` file should be updated with the corresponding information. **Never delete** rows (rats) from the archive, simply update their `end_date` column. 
	
## Design
Initial whiteboard - https://miro.com/app/board/uXjVO2HtI6U=/

The basic architecture is Model-View-Controller.
#### Model
The `*_archive.Rdata` files contain the model, i.e. the storage of data. These include the `rat_archive` (CSV not Rdata), the `run_archive`, and the per-experiment trial archives (e.g. `Fmr1-LE_archive`).

#### View
The primary view is provided by `supervisor-summarize.R`. This script outputs a well-formatted .xlsx (Excel) file that allows the experimental supervisor to view summary data and statistics for the last several days for each rat, along with any warnings found when processing each day's files.

#### Controller
The primary controller is `main.R`, used through its frontend interface `app.R`. This file imports `.mat` files, which are the output from each run of the  matlab control program. The data is converted into usable formats and the file is extensively checked for errors, especially mismatches between the expected or assigned parameters of that experimental run to the actual parameters used by the computer during the run. Data from the run's `.mat` file and input from `app.R` (such as the animal's current weight) is saved into the appropriate archives.

The `supervisor-assign.R` file, combined with the summary .xlsx file, also acts as a controller. This script transfers the future assignments (created by the supervisor using the summary sheet) to the model. This data is then checked against during `main.R`'s execution.

#### Other files
- `settings.R` - contains settings used by the main and supervisor scripts, such as the TH Cutoff, the desired minimum number of completed trials, or the amount of weight change required to trigger a warning message.
- `experiment_details.csv` - This file lists the active experiments, the phases that correspond to each experiment, and the tasks and details that correspond to each phase. Combined, this nomenclature specifies most of the parameters used to create the configuration files that are loaded into the matlab control program that specify the behavior of the experimental boxes. Future work is intended to use these parameters to generate the matlab configuration files directly; in 0.1-ALPHA these values are used for post-run validation instead.
- `graphing unrolled.R` - contains graph generation used by the shiny app.

#### Admin Tools
Several small scripts are available for those comfortable with R. Note that these have *not* been made user friendly or robust. **Use at your own risk.**
- `Load enviornment.R` - This should be used following a cleaning of the global environment. *It can be out of sync from main*
- `Common lookups.R` - This contain short scripts that filter run_archive or rat_archive for information that I have looked up several times. These have not been made into functions.
- `Modify an entry.R` - This contains a quick way to set a run invalid, change the weight or assignment. It should be kept in the **safe** state i.e. comment out from = to the new value.
- `remove entry.R` and `Clean NA_archive entries.R` - These are used to clean poor entries. They are under active development to make them safe for others to use. Remove entry has been made into a function that backs-up deleted data and requires confrimation to use.
- `import_old_excel.R` - This is a utility to read a directory tree and import information from the old experimental excel sheets that PiedPiper is designed to replace.

