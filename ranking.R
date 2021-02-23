csbDf <- knotsAndSpikes(t_start = 0, t_end = 40, knots = c(14,20), spikes = c(14,20))

training_period_start <- as.factor("2020-08-30")
training_period_end <- as.factor("2020-02-22")
Target.name <- "outcomes"
feature.names <- c("case_avg", "absent_rate","dropout_rate","enrollment","daily_atten",
                   "expend_federal", "expend_state","expenditures", "s_deaths",
                   "s_cases")
schooldata$s_cases <- as.factor(schooldata$s_cases)
schooldata$s_deaths <- as.factor(schooldata$s_deaths)
schooldata$case_avg <- as.factor(schooldata$case_avg)
schooldata[] <- lapply(schooldata, function(x) as.factor(x))
schooldata <- schooldata[-c(16,22)]
schooldata$pred_logit <- rankfunc(schooldata) 

group_as <- "deciles"
if(group_as == 'deciles'){
  schooldata<- deciles(schooldata)
  
}else{
  schooldata <- twentiles(schooldata)
  
}

# ROC Curve
par(pty = "s")
print(roc(schooldata$outcomes, schooldata$p_outcomes, plot = TRUE,
          legacy.axes = TRUE, print.auc = TRUE, col = "blue", lwd =1))
#Set legend
legend("bottomright", legend = "virtual", col= "blue", lwd = 1)

view(schooldata)
