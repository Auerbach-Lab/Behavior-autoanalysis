
# Package loading ---------------------------------------------------------

# data loading/manipulation
library(R.matlab)

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
file = readMat("~/GitHub/Behavior-autoanalysis/Only_2by2table.mat")

# > file
# $source.list
# $source.list[[1]]
# $source.list[[1]][[1]]
# [,1]
# [1,] "tone"
#
#
# $source.list[[2]]
# $source.list[[2]][[1]]
# [,1]
# [1,] "tone"
#
#
# $source.list[[3]]
# $source.list[[3]][[1]]
# [,1]
# [1,]   30
#
#
# $source.list[[4]]
# $source.list[[4]][[1]]
# [,1]
# [1,]   40
#
#
#
# attr(,"header")
# attr(,"header")$description
# [1] "MATLAB 5.0 MAT-file, Platform: PCWIN64, Created on: Sat Jun 11 13:15:37 2022                                        "
#
# attr(,"header")$version
# [1] "5"
#
# attr(,"header")$endian
# [1] "little"

class(file$source.list)
# > class(file$source.list)
# [1] "list"

print(file$source.list)
# > print(file$source.list)
# [[1]]
# [[1]][[1]]
# [,1]
# [1,] "tone"
#
#
# [[2]]
# [[2]][[1]]
# [,1]
# [1,] "tone"
#
#
# [[3]]
# [[3]][[1]]
# [,1]
# [1,]   30
#
#
# [[4]]
# [[4]][[1]]
# [,1]
# [1,]   40




View(file$source.list)


