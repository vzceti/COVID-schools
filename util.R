# load required packages, set data sources, track run time 
a <- Sys.time()

source("functions.R")

requireLibs(c("tidyverse","readr","tidyr","dplyr","reshape2",
              "plyr","zoo","readxl"))

changes <- "changedates.xlsx"
changes1 <- "changedates1.csv"
nytlink <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
state <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"

source("fe.R")

rm(list=setdiff(ls(), c("a", "schooldata")))

#source("ranking.R")
# write.csv(schooldata, "output.csv")

a <- Sys.time()-a
a

#comparing my code to Molly's: input is simple, is input2 actually feature engineering
#next step: rank ordering model? only one variable
#next step: 