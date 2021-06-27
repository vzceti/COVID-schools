input_start <- schooldata[which(schooldata$t == 0),]
schooldata$changedate <- as.Date(as.numeric(schooldata$changedate))

schooldata$expend_federal <- as.numeric(schooldata$expend_federal)
schooldata$expend_state <- as.numeric(schooldata$expend_state)
schooldata$unemployed <- as.numeric(schooldata$unemployed)
schooldata$daily_atten <- as.numeric(schooldata$daily_atten)
schooldata$dropout_rate <- as.numeric(schooldata$dropout_rate)
schooldata$changedate <- as.numeric(schooldata$changedate)

fit_start <- glm(startstatus ~ expenditures + case_avg + trump + pop,
                 data=input_start, family='gaussian')
summary(fit_start)

install.packages("Hmisc")
library("Hmisc")
x <- as.matrix(schooldata[c(2:4,13,7,14)])
cor(x)

install.packages("corrplot")
library(corrplot)
corrplot(cor(x), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
