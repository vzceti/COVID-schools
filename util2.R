# load required packages, set data sources, track run time 
a <- Sys.time()

require(tidyverse)
require(readr)
require(tidyr)
require(dplyr)
require(reshape2)
require(plyr)
require(zoo)
require(readxl)

inputdata <- "data2.csv"
changes <- "changedates.xlsx"
nytlink <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

source("input.R")

rm(list=setdiff(ls(), c("a", "schooldata")))

a <- Sys.time()-a
a
