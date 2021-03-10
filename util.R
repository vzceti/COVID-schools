a <- Sys.time()

source("functions.R")

requireLibs(c("tidyverse","readr","tidyr","dplyr","reshape2",
              "plyr","zoo","readxl","pROC","data.table", "MASS"))
source("fe.R")

source("ranking.R")

a <- Sys.time()-a
a
