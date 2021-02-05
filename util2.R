# load required packages, set data sources, track run time 
a <- Sys.time()

source("functions.R")

requireLibs(c("tidyverse","readr","tidyr","dplyr","reshape2",
              "plyr","zoo","readxl"))

changes <- "changedates.xlsx"
changes1 <- "changedates1.csv"
nytlink <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

source("input2.R")

rm(list=setdiff(ls(), c("a", "schooldata")))
view(schooldata)

# write.csv(schooldata, "output.csv")

a <- Sys.time()-a
a