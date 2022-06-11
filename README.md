# Behavior-autoanalysis

Initial design whiteboard:
https://miro.com/app/board/uXjVO2HtI6U=/

TODO:
- get renv working. See https://rstudio.github.io/renv/articles/renv.html


Stim.Source_list (a 'cell array') isn't being imported with its dimensions 
For the example 'non-working' file it should be 9 wide by 29 long
Plan to contact https://github.com/HenrikBengtsson/R.matlab about it
The list, which is how it imports, runs down the columns (check item 29, stim$source.list[[29]] == 1,29 in matlab)
This appears to happen to any import of matlab type 'cell array'. I have 2 simplified versions (down to only the single cell) and all import as 1 dimensional lists.
In every case, opening them in the matlab workspace retains the dimensions.
