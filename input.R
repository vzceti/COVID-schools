# THUS BEGINS data cleaning and data wrangling... let's get this bread
schooldata <- read.csv(inputdata)
colnames(schooldata) <- c("divnum", "county", "date", "status", "specifics", "membership")
schooldata$date <- as.Date(schooldata$date, format = "%B %d")
startdata <- subset(schooldata, date == "2020-09-08", -c(1,3,5,6))
date <- rep(as.Date("2020-09-06"),length(startdata$county))
startdata<- cbind(startdata,date)
rownames(startdata) <- c()
index <- rep(0,length(startdata$county))
startdata <- cbind(startdata, index)
# now you have the beginning status of each school district

# change of status date
changed <- read_excel(changes)
changed <- changed[-c(2,5,6)]
colnames(changed) <- c(colnames(startdata))
changed$date <- as.Date(changed$date, format = "%B %d")
index <- round((changed$date - as.Date("2020-09-06"))/7)
changed <- cbind(changed,index)

# bind the two together
schooldata <- rbind(startdata, changed)
rownames(schooldata) <- c()
schooldata <- group_by(schooldata, county)

# set statuses to numbers, 1 being least restrictive, 5 being the most
schooldata <- mutate(schooldata, status = case_when(
  status == "Fully Remote" ~ 1,
  status == "Partial Hybrid" ~ 2,
  status == "All Hybrid" ~ 3,
  status == "Partial in Person" ~ 4,
  status == "In Person" ~ 5)
)

#seven-day rolling average
nytdata <- read_csv(url(nytlink))
nytdata <- subset(nytdata, state == "Virginia",-c(3,4))
averages <- nytdata %>%
  dplyr::arrange(desc(county)) %>% 
  dplyr::group_by(county) %>% 
  dplyr::mutate(cases_7 = zoo::rollmean(cases, k = 7, fill = NA),
                deaths_7 = zoo::rollmean(deaths, k = 7, fill = NA)) %>% 
  dplyr::ungroup()
averages <- averages[averages["date"] >= "2020-09-06", ]
averages <- group_by(averages, week = cut(date, "week"))
weekdays <- weekdays(averages$date)
averages <- cbind(averages, weekdays)
averages <- subset(averages, weekdays == "Sunday", -c(3,4,7,8))
rownames(averages) <- c()
# now you have the weekly seven-day averages

remote <- ifelse(schooldata$status == 1,1,0)
schooldata <- cbind(schooldata, remote)
colnames(schooldata) <- c("county", "state", "status", "index", "remote")

#bind the two together
