# load required packages, set data sources, track run time 
a <- Sys.time()

require(readxl)
require(tidyverse)
require(readr)

inputdata <- "data1.xlsx"
nytlink <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

# THUS BEGINS data cleaning and data wrangling... let's get this bread

#SEPTEMBER 8
#read school data, add sheet name column, and remove two districts
sheet0908 <- read_excel(inputdata, sheet = 1)
data2020908 <- do.call(
  rbind, lapply(sheet0908, function(x) 
    transform(readxl::read_excel(inputdata, sheet = 1), date = "2020/09/08")))
colnames(data2020908) <- c("divnum", "county", "status", "specifics", "date")
data2020908 <- unique(data2020908)
data2020908 <- data2020908[data2020908$county != "West Point" & 
                             data2020908$county != "Colonial Beach",]
rownames(data2020908) <- c()

#read NYT data, subset for date and state, set aside county names, find index
#for counties to transform
nytdata0908 <- read_csv(url(nytlink))
nytdata0908 <- subset(nytdata0908, date == "2020-09-08" & state == "Virginia",
                      select = -c(1, 3:4))
greensville <- grep("Greensville", nytdata0908$county)
emporia <- grep("Emporia city", nytdata0908$county)
fairfax <- grep("Fairfax", nytdata0908$county)
fairfaxcity <- grep("Fairfax city", nytdata0908$county)
williamsburg <- grep("Williamsburg city", nytdata0908$county)
jamescity <- grep("James City", nytdata0908$county)
nytcounty08 <- nytdata0908$county[-c(emporia, fairfaxcity, jamescity)]

#consolidate counties to school districts and bind back together
nytdata0908 <- nytdata0908[-1] 
colnames(nytdata0908) <- c("cases", "deaths")
nytdata0908[greensville,] <- nytdata0908[emporia,] + nytdata0908[greensville,]
nytdata0908[fairfax,] <- colSums(nytdata0908[fairfax,])
nytdata0908[williamsburg,] <- nytdata0908[williamsburg,] + nytdata0908[jamescity,]
deaths <- nytdata0908$deaths[-c(emporia, fairfaxcity, jamescity)]
cases <- nytdata0908$cases[-c(emporia, fairfaxcity, jamescity)]
nytdata98 <- cbind(nytcounty08, deaths, cases)

#bind school data to NYT COVID data and remove county column
alldata0908 <- cbind(data2020908, nytdata98)
alldata0908 = subset(alldata0908, select = -6)

#SEPTEMBER 22
#read school data, add sheet name column, and remove two districts
sheet0922 <- read_excel(inputdata, sheet = 1)
data2020922 <- do.call(
  rbind, lapply(sheet0922, function(x) 
    transform(readxl::read_excel(inputdata, sheet = 1), date = "2020/09/22")))
colnames(data2020922) <- c("divnum", "county", "status", "specifics", "date")
data2020922 <- unique(data2020922)
rownames(data2020922) <- c()
data2020922 <- data2020922[data2020922$county != "West Point" & 
                             data2020922$county != "Colonial Beach",]

#read NYT data, subset for date and state, set aside county names, find index
#for counties to transform
nytdata0922 <- read_csv(url(nytlink))
nytdata0922 <- subset(nytdata0922, date == "2020-09-22" & state == "Virginia",
                      select = -c(1, 3:4))
greensville <- grep("Greensville", nytdata0922$county)
emporia <- grep("Emporia city", nytdata0922$county)
emporia
fairfax <- grep("Fairfax", nytdata0922$county)
fairfaxcity <- grep("Fairfax city", nytdata0922$county)
williamsburg <- grep("Williamsburg city", nytdata0922$county)
jamescity <- grep("James City", nytdata0922$county)
nytcounty <- nytdata0922$county[-c(emporia, fairfaxcity, jamescity)]

#consolidate counties to school districts and bind back together
nytdata0922 <- nytdata0922[-1] 
colnames(nytdata0922) <- c("cases", "deaths")
nytdata0922[greensville,] <- nytdata0922[emporia,] + nytdata0922[greensville,]
nytdata0922[fairfax,] <- colSums(nytdata0922[fairfax,])
nytdata0922[williamsburg,] <- nytdata0922[williamsburg,] + nytdata0922[jamescity,]
deaths <- nytdata0922$deaths[-c(emporia, fairfaxcity, jamescity)]
cases <- nytdata0922$cases[-c(emporia, fairfaxcity, jamescity)]
nytdata922 <- cbind(nytcounty, deaths, cases)

#consolidate counties to school districts and bind back together
alldata0922 <- cbind(data2020922, nytdata922)
alldata0922 = subset(alldata0922, select = -6)

#bind school data to NYT COVID data and remove county column
schooldata <- rbind(alldata0908, alldata0922)
rownames(schooldata) <- c()
schooldata$date <- as.Date(schooldata$date)

#clean up all obsolete data frames and vectors
rm("alldata0908", "alldata0922", "cases", "data2020908", "data2020922", 
   "deaths", "emporia", "fairfax", "fairfaxcity", "greensville", 
   "inputdata", "jamescity", "nytcounty", "nytdata0908", "nytdata0922", 
   "nytdata922", "nytdata98", "nytlink", "sheet0908", "sheet0922", 
   "williamsburg", "nytcounty08")

Sys.time()-a