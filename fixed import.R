
# Package loading ---------------------------------------------------------

# data loading/manipulation
# library(R.matlab)
library(R.methodsS3)
library(R.oo)
library(R.utils)
library(utils)
library(methods)


# Running environment -----------------------------------------------------

# from print(sessionInfo())
# R version 4.1.2 (2021-11-01)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
#
# Matrix products: default
#
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#   [1] R.matlab_3.6.2


# Load file ---------------------------------------------------------------
source("~/GitHub/Behavior-autoanalysis/R/000.R")
source("~/GitHub/Behavior-autoanalysis/R/006.fixVarArgs.R")
source("~/GitHub/Behavior-autoanalysis/R/utils.R")
source("~/GitHub/Behavior-autoanalysis/R/zzz.R")
source("~/GitHub/Behavior-autoanalysis/R/readMat.R")
file = readMat("A:\\coding\\matlab\\2by3cellarray.mat",verbose=-100,drop=TRUE)
class(file$source.list)
print(file$source.list)
View(file$source.list)


