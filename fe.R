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
startdate <- "2020-08-30"

absent <- read.csv("absent.csv")
absent <- absent[-c(1,3:5,7:8)]
colnames(absent) <- c("county","absent_rate")
absent <- aggregate(absent$absent_rate,by=list(absent$county), FUN=mean)
absent <- absent[-c(28,126),]
rownames(absent) <- c()
colnames(absent) <- c("county","absent_rate")

dropout <- read.csv("dropout.csv")
dropout <- dropout[-c(1,3:7)]
colnames(dropout) <- c("county","dropout_rate")
dropout$dropout_rate <- as.numeric(dropout$dropout_rate)
dropout <- aggregate(dropout$dropout_rate,by=list(dropout$county), FUN=mean)
dropout <- dropout[-c(28,126),]
colnames(dropout) <- c("county","dropout_rate")
newrow <- c("Lexington",NA)
dropout <- rbind(dropout[1:67,],newrow,dropout[-(1:67),])

enrollment <- read.csv("enrollment.csv")
enrollment <- enrollment[-c(1:2,4)]
colnames(enrollment) <- c("county","number")
enrollment <- aggregate(enrollment$number,by=list(enrollment$county), FUN=sum)
enrollment <- enrollment[-c(28,35,124,128),]
colnames(enrollment) <- c("county","enrollment")

spending <- read.csv("spending.csv")
spending <- spending[-c(1,2)]
spending <- spending[-c(28,125),]
newrow <- c("Lynchburg",rep(NA,ncol(spending)-1))
spending <- rbind(spending[1:71,],newrow,spending[-(1:71),])

expenditures <- read.csv("expenditures.csv")
expenditures <- expenditures[-c(1,3)]
colnames(expenditures) <- c("county", "expenditures")
expenditures <- expenditures[-c(28,126),]

changed <- cbind(changed,absent,dropout,enrollment,spending,expenditures)
changed <- changed[-c(7,9,11,13,15:17,20,21:26)]
colnames(changed) <- c("county","startstatus","status1","status2","date1","date2",
                       "absent_rate","dropout_rate","enrollment","daily_atten",
                       "expend_federal","expend_state","expenditures")
rm("absent","dropout","enrollment","expenditures","spending")

state <- read_csv(url(state))
state <- subset(state, state == "Virginia")
averages2 <- state %>%
  dplyr::arrange(desc(state)) %>% 
  dplyr::group_by(state) %>% 
  dplyr::mutate(cases_7 = zoo::rollmean(cases, k = 7, fill = NA),
                deaths_7 = zoo::rollmean(deaths, k = 7, fill = NA)) %>% 
  dplyr::ungroup()
averages2 <- averages2[averages2["date"] >= startdate, ]
averages2 <- group_by(averages2, week = cut(date, "week"))
weekdays <- weekdays(averages2$date)
weekdays <- as.data.frame(weekdays)
colnames(weekdays) <- "weekdays"
averages2 <- cbind(averages2, weekdays)
averages2 <- subset(averages2, weekdays == "Sunday", -c(3,4,7,8))
rownames(averages2) <- c()
averages2 <- averages2[-c(2,5)]
averages2 <- subset(averages2, averages2$date >= startdate)
averages2 <- as.data.frame(averages2)

t <- c(0:40)
date <- c(as.Date(startdate)+c(7*(0:40)))
startweek <- rep(startdate,length(t))
changedate <- c(rep("NULL",length(t)))
tiny <- cbind(t,date,startweek)
tiny <- as.data.frame(tiny)
tiny$date <- as.double(tiny$date)
tiny$date <- as.Date(tiny$date)
tiny <- full_join(tiny, averages2, by = "date")
colnames(tiny) <- c("t","date","startweek","s_deaths","s_cases")
schooldata <- merge(changed,tiny,all=TRUE)
rm("tiny","changed","averages2")

schooldata$startstatus <- setthestatus(schooldata$startstatus)
schooldata$status1 <- setthestatus(schooldata$status1)
schooldata$status2 <- setthestatus(schooldata$status2)

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
colnames(averages) <- c("date","case_avg","death_avg", "county")
schooldata <- full_join(schooldata, averages, by = c("date","county"))

schooldata <- cbind(schooldata, as.data.frame(rep(0, length(schooldata$county))),
                    as.data.frame(rep(0, length(schooldata$county))))
names(schooldata)[20] <- "outcomes"
names(schooldata)[21] <- "changedate"
schooldata$outcomes <- ifelse((schooldata$startstatus >= 1 
                    & (schooldata$status1 == 1|schooldata$status2 == 1)),1,0)
schooldata$changedate <- ifelse(schooldata$outcomes ==1 & 
                      schooldata$status2<schooldata$status1, 
                      schooldata$date2, schooldata$date1)
schooldata$changedate <- as.Date(schooldata$changedate)
schooldata$outcomes <- ifelse(!is.na(schooldata$changedate)&
                              schooldata$changedate>schooldata$date,
                              0, 1)
schooldata <- subset(schooldata,!(schooldata$startstatus == 1 
                    & schooldata$status1 == 1
                    & schooldata$status2 == 1))
schooldata <- cbind(schooldata, as.data.frame(rep(0, length(schooldata$county))))
names(schooldata)[22] <- "filter"
schooldata$filter <- (schooldata$changedate + 7)
schooldata$filter <- ifelse(!is.na(schooldata$filter)&
                                schooldata$filter>schooldata$date,
                              0, 1)
schooldata <- subset(schooldata, !(schooldata$filter ==1))
schooldata <- schooldata[-23]

cases <- as.data.frame(table(schooldata$date))
names(cases) <- c("dates","frequency")
cases <- cbind(cases,c(1:length(cases$dates)))
names(cases) <- c("dates","frequency","t")

view(schooldata)
plot <- ggplot(cases, aes(x=t, y=frequency)) + geom_point()
