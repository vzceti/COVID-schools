# load required packages, set data sources, track run time 
a <- Sys.time()

source("functions.R")

requireLibs(c("tidyverse","readr","tidyr","dplyr","reshape2",
              "plyr","zoo","readxl","pROC"))

changes <- "changedates.xlsx"
changes1 <- "changedates1.csv"
nytlink <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
state <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"

source("fe.R")

source("ranking.R")

# write.csv(schooldata, "output.csv")
# rm(list=setdiff(ls(), c("a", "schooldata")))

a <- Sys.time()-a
a
