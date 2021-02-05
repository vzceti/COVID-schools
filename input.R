# THUS BEGINS data cleaning and data wrangling... let's get this bread
# read in start statuses for each school district (VDOE)
schooldata <- read.csv(inputdata)
colnames(schooldata) <- c("divnum", "county", "date", "startstatus", "specifics", "membership")
schooldata <- mutate(schooldata, date = case_when(
  date == "September 8" ~ as.Date("2020-09-08"),
  date == "September 22" ~ as.Date("2020-09-22"),
  date == "November 12" ~ as.Date("2020-09-22"),
))
schooldata$date <- as.Date(schooldata$date, format = "%B %d")
startdata <- subset(schooldata, date == "2020-09-08", -c(1,3,5,6))
rownames(startdata) <- c()

# tiny dataset for cartesian join
# set start date, number of weeks, and days of the week - then merge
startdate <- "2020-08-30"
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
# read in manufactured dataset, create index for change dates
# set change date for each school
changed <- read_excel(changes)
changed <- changed[-c(2,5,6)]
colnames(changed) <- c("county","status1","changedate1")
# index1 <- round((as.Date(changed$changedate1) - (as.Date("2020-09-06")))/7)
# changed <- cbind(changed, index1)
schooldata <- merge(schooldata, changed, by = "county", all.x = TRUE)

#set second change date
changed1 <- read.csv(changes1)
changed1 <- changed1[-c(2,3,6,7,8,9,10)]
colnames(changed) <- c("county","status2","changedate2")
# index2 <- round((as.Date(changed1$changedate2) - (as.Date("2020-09-06")))/7)
# changed1 <- cbind(changed1, index2)
schooldata <- merge(schooldata, changed1, by = "county", all.x = TRUE)
colnames(schooldata) <- c("county", "startstatus", "t", "tweek", "startdate", 
                          "status1", "date1","status2", "date2")

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

# set statuses to numbers, 1 being least restrictive, 5 being the most
schooldata <- mutate(schooldata, status2 = case_when(
  status2 == "Fully Remote" ~ 1,
  status2 == "Partial Hybrid" ~ 2,
  status2 == "All Hybrid" ~ 3,
  status2 == "Partial in Person" ~ 4,
  status2 == "In Person" ~ 5)
)

# add column for binary outcomes (if it changes, if it goes to remote)
outcome <- rep(0, length(schooldata$county))
outcome <- ifelse(schooldata$date1>schooldata$tweek, 1, 0)
outcome[is.na(outcome)] <- 0
schooldata <- cbind(schooldata,outcome)
schooldata <- schooldata[order(schooldata$county, schooldata$tweek),]
remote <- as.data.frame(ifelse(schooldata$status == 1,1,0))
colnames(remote) <- "remote"
remote[is.na(remote)] <- 0
schooldata <- cbind(schooldata, remote)

#seven-day rolling average for cases and deaths
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
weekdays <- as.data.frame(weekdays)
colnames(weekdays) <- "weekdays"
averages <- cbind(averages, weekdays)
averages <- subset(averages, weekdays == "Sunday", -c(3,4,7,8))
rownames(averages) <- c()

averages <- averages[averages$county != "Emporia city"&
                          averages$county != "James City" & 
                          averages$county != "Fairfax city",]
schooldata <- schooldata[schooldata$county != "West Point" &
                          schooldata$county != "Colonial Beach",]
averagelevels <- sort(unique(averages$county))
schoollevels <- sort(unique(schooldata$county))
tiny <- as.data.frame(c(1:(nrow(averages)/130)))
schoollevels <- merge(schoollevels,tiny)
schoollevels <- schoollevels[-2]
schoollevels <- sort(schoollevels$x)
averages$county <- sort(averages$county)
averages <- cbind(averages, schoollevels)
averages <- averages[-2]
colnames(averages) <- c("tweek","case_avg","death_avg", "county")
schooldata <- full_join(schooldata, averages, by = c("tweek","county"))
schooldata <- schooldata[-8]