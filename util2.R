# ctrl-F "MODIFY" for adding data or customizing to personal machine
# load required packages, set data sources, track run time 
a <- Sys.time()

require(readxl)
require(tidyverse)
require(readr)

# MODIFY inputdata path
inputdata <- "data2.csv"
nytlink <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

source("input.R")

# TIME FOR feature engineering


Sys.time()-a
