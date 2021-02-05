changed <- read_excel(changes)
changed <- changed[-c(2,5,6)]
changed1 <- read.csv(changes1)
changed1 <- changed1[-c(6:10)]
changed <- merge(changed1, changed, by = "county", all.x = TRUE)
changed <- changed[-6]
rm("changed1")
colnames(changed) <- c("county","startstatus","status1","status2","date2","date1")
changed <- changed[,c(1:4,6,5)]
changed$date2 <- as.Date(changed$date2,"%m/%d/%Y")
# schooldata <- merge(schooldata, changed, by = "county", all.x = TRUE)
startdate <- "2020-08-30"

t <- c(0:40)
dates <- c(as.Date(startdate)+c(7*(0:40)))
startweek <- rep(startdate,length(t))
changedate <- c(rep("NULL",length(t)))
tiny <- cbind(t,dates,startweek)
tiny <- as.data.frame(tiny)
tiny$dates <- as.double(tiny$dates)
tiny$dates <- as.Date(tiny$dates)
schooldata <- merge(changed,tiny,all=TRUE)
rm("tiny","changed")

schooldata$startstatus <- setthestatus(schooldata$startstatus)
schooldata$status1 <- setthestatus(schooldata$status1)
schooldata$status2 <- setthestatus(schooldata$status2)
colnames(schooldata) <- c("county", "startstatus", "status1", "status2", 
                          "date1", "date2", "t", "tweek", "startweek" )

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
schooldata <- schooldata[-12]

schooldata <- cbind(schooldata, as.data.frame(rep(0, length(schooldata$county))),
                    as.data.frame(rep(0, length(schooldata$county))))
names(schooldata)[12] <- "outcomes"
names(schooldata)[13] <- "changedate"
schooldata$outcomes <- ifelse((schooldata$startstatus >= 1 
                    & (schooldata$status1 == 1|schooldata$status2 == 1)),1,0)
schooldata$changedate <- ifelse(schooldata$outcomes ==1 & 
                      schooldata$status2<schooldata$status1, 
                      schooldata$date2, schooldata$date1)
schooldata$changedate <- as.Date(schooldata$changedate)
schooldata$outcomes <- ifelse(!is.na(schooldata$changedate)&
                              schooldata$changedate>schooldata$tweek,
                              0, 1)
# schooldata <- subset(schooldata, schooldata$startstatus == 1 
#                       & schooldata$status1 == 1
#                       & schooldata$status2 == 1){subset(schooldata)}
# view(schooldata)

#set fully restrictive ones aside
#once they go more restrictive, (all future 1's), no more records
#predictor: how restrictive at beginning? (caution)
#probability of school districts going remote over time
#set up filter
#add Virginia overall trend (https://github.com/nytimes/covid-19-data/blob/master/us-states.csv)