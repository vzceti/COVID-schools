# load required packages, set data sources, track run time 
a <- Sys.time()

requireLibs <- function(libs) {
  for (lib in libs){
    if(!is.element(lib, .packages(all.available = TRUE))) {
      install.packages(lib)
    }
    library(lib, character.only = TRUE)
  }
}
requireLibs(c("tidyverse","readr","tidyr","dplyr","reshape2",
              "plyr","zoo","readxl"))

inputdata <- "data2.csv"
changes <- "changedates.xlsx"
nytlink <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

source("input.R")

rm(list=setdiff(ls(), c("a", "schooldata")))
view(schooldata)

a <- Sys.time()-a
a
