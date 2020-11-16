# THUS BEGINS data cleaning and data wrangling... let's get this bread
schooldata <- read.csv(inputdata)
colnames(schooldata) <- c("divnum", "county", "date", "status", "specifics", "membership")
schooldata$date <- as.Date(schooldata$date, format = "%B %d")
schooldata <- schooldata[schooldata$county != "West Point" & 
                           schooldata$county != "Colonial Beach",]
rownames(schooldata) <- c()
data908 <- subset(schooldata, date == "2020-09-08")
data922 <- subset(schooldata, date == "2020-09-22")
data1112 <- subset(schooldata, date == "2020-11-12")
# MODIFY - add new dates here

# SEPTEMBER 8 read NYT data, subset for date and state, set aside county names, find index
# for counties to transform
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

# consolidate counties to school districts and bind back together
nytdata0908 <- nytdata0908[-1] 
colnames(nytdata0908) <- c("cases", "deaths")
nytdata0908[greensville,] <- nytdata0908[emporia,] + nytdata0908[greensville,]
nytdata0908[fairfax,] <- colSums(nytdata0908[fairfax,])
nytdata0908[williamsburg,] <- nytdata0908[williamsburg,] + nytdata0908[jamescity,]
deaths <- nytdata0908$deaths[-c(emporia, fairfaxcity, jamescity)]
cases <- nytdata0908$cases[-c(emporia, fairfaxcity, jamescity)]
nytdata98 <- cbind(nytcounty08, deaths, cases)

# SEPTEMBER 22 read NYT data, subset for date and state, set aside county names, find index
# for counties to transform
nytdata0922 <- read_csv(url(nytlink))
nytdata0922 <- subset(nytdata0922, date == "2020-09-22" & state == "Virginia",
                      select = -c(1, 3:4))
greensville <- grep("Greensville", nytdata0922$county)
emporia <- grep("Emporia city", nytdata0922$county)
fairfax <- grep("Fairfax", nytdata0922$county)
fairfaxcity <- grep("Fairfax city", nytdata0922$county)
williamsburg <- grep("Williamsburg city", nytdata0922$county)
jamescity <- grep("James City", nytdata0922$county)
nytcounty22 <- nytdata0922$county[-c(emporia, fairfaxcity, jamescity)]

# consolidate counties to school districts and bind back together
nytdata0922 <- nytdata0922[-1] 
colnames(nytdata0922) <- c("cases", "deaths")
nytdata0922[greensville,] <- nytdata0922[emporia,] + nytdata0922[greensville,]
nytdata0922[fairfax,] <- colSums(nytdata0922[fairfax,])
nytdata0922[williamsburg,] <- nytdata0922[williamsburg,] + nytdata0922[jamescity,]
deaths <- nytdata0922$deaths[-c(emporia, fairfaxcity, jamescity)]
cases <- nytdata0922$cases[-c(emporia, fairfaxcity, jamescity)]
nytdata922 <- cbind(nytcounty22, deaths, cases)

# NOVEMBER 12 read NYT data, subset for date and state, set aside county names, find index
# for counties to transform
nytdata1112 <- read_csv(url(nytlink))
nytdata1112 <- subset(nytdata1112, date == "2020-11-12" & state == "Virginia",
                      select = -c(1, 3:4))
greensville <- grep("Greensville", nytdata1112$county)
emporia <- grep("Emporia city", nytdata1112$county)
fairfax <- grep("Fairfax", nytdata1112$county)
fairfaxcity <- grep("Fairfax city", nytdata1112$county)
williamsburg <- grep("Williamsburg city", nytdata1112$county)
jamescity <- grep("James City", nytdata1112$county)
nytcounty12 <- nytdata1112$county[-c(emporia, fairfaxcity, jamescity)]

# consolidate counties to school districts and bind back together
nytdata1112 <- nytdata1112[-1] 
colnames(nytdata1112) <- c("cases", "deaths")
nytdata1112[greensville,] <- nytdata1112[emporia,] + nytdata1112[greensville,]
nytdata1112[fairfax,] <- colSums(nytdata1112[fairfax,])
nytdata1112[williamsburg,] <- nytdata1112[williamsburg,] + nytdata1112[jamescity,]
deaths <- nytdata1112$deaths[-c(emporia, fairfaxcity, jamescity)]
cases <- nytdata1112$cases[-c(emporia, fairfaxcity, jamescity)]
nytdata1112 <- cbind(nytcounty12, deaths, cases)

# bind school data to NYT COVID data and remove county column
data908 <- cbind(data908, nytdata98)
data908 = subset(data908, select = -7)
data922 <- cbind(data922, nytdata922)
data922 = subset(data922, select = -7)
data1112 <- cbind(data1112, nytdata1112)
data1112 = subset(data1112, select = -7)

# bind dates together
schooldata <- rbind(data908, data922, data1112)
rownames(schooldata) <- c()
schooldata$date <- as.Date(schooldata$date)

# clean up all obsolete data frames and vectors
rm(list=setdiff(ls(), c("a", "schooldata")))
