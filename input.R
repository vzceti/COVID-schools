# THUS BEGINS data cleaning and data wrangling... let's get this bread
schooldata <- read.csv(inputdata)
startdate <- "2020-08-30"
colnames(schooldata) <- c("divnum", "county", "date", "startstatus", "specifics", "membership")
schooldata$date <- as.Date(schooldata$date, format = "%B %d")
startdata <- subset(schooldata, date == "2020-09-08", -c(1,3,5,6))
rownames(startdata) <- c()

# tiny dataset for cartesian join
t <- c(0:40)
dates <- c(as.Date(startdate)+c(7*(0:40)))
startweek <- rep(startdate,length(t))
changedate <- c(rep("NULL",length(t)))
tiny <- cbind(t,dates,startweek)
tiny <- as.data.frame(tiny)
tiny$dates <- as.double(tiny$dates)
tiny$dates <- as.Date(tiny$dates)
schooldata <- merge(startdata,tiny,all=TRUE)

# change of status date
changed <- read_excel(changes)
changed <- changed[-c(2,5,6)]
colnames(changed) <- c("county","status1","changedate1")

# find changedate
schooldata <- merge(schooldata, changed, by = "county", all.x = TRUE)
colnames(schooldata) <- c("county", "startstatus", "t", "tweek", "startdate", "status1", "date1")

# set statuses to numbers, 1 being least restrictive, 5 being the most
schooldata <- mutate(schooldata, startstatus = case_when(
  startstatus == "Fully Remote" ~ 1,
  startstatus == "Partial Hybrid" ~ 2,
  startstatus == "All Hybrid" ~ 3,
  startstatus == "Partial in Person" ~ 4,
  startstatus == "In Person" ~ 5)
)

schooldata <- mutate(schooldata, status1 = case_when(
  status1 == "Fully Remote" ~ 1,
  status1 == "Partial Hybrid" ~ 2,
  status1 == "All Hybrid" ~ 3,
  status1 == "Partial in Person" ~ 4,
  status1 == "In Person" ~ 5)
)

outcome <- rep(0, length(schooldata$county))
outcome <- ifelse(schooldata$date1>schooldata$tweek, 1, 0)
outcome[is.na(outcome)] <- 0

schooldata <- cbind(schooldata,outcome)
schooldata <- schooldata[order(schooldata$county, schooldata$tweek),]

#seven-day rolling average
nytdata <- read_csv(url(nytlink))
nytdata <- subset(nytdata, state == "Virginia",-c(3,4))
averages <- nytdata %>%
  dplyr::arrange(desc(county)) %>% 
  dplyr::group_by(county) %>% 
  dplyr::mutate(cases_7 = zoo::rollmean(cases, k = 7, fill = NA),
                deaths_7 = zoo::rollmean(deaths, k = 7, fill = NA)) %>% 
  dplyr::ungroup()
averages <- averages[averages["date"] >= startdate, ]
averages <- group_by(averages, week = cut(date, "week"))
weekdays <- weekdays(averages$date)
averages <- cbind(averages, weekdays)
averages <- subset(averages, weekdays == "Sunday", -c(3,4,7,8))
rownames(averages) <- c()
# now you have the weekly seven-day averages
