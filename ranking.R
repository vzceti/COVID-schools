csbDf <- knotsAndSpikes(t_start = 0, t_end = 40, knots = c(10,20,23), spikes = 19)

input_start <- schooldata[which(schooldata$t == 20 ),]

fit_start <- glm(status2 ~ trump + pop + case_avg + s_cases,
                 data=input_start, family='gaussian')

summary(fit_start)
# 
# scoring_data_strip$p_fail <- predict(fit_fail, newdata=scoring_data_strip, type='response')
# 
# input_retire <- scoring_data_strip[which(  scoring_data_strip$failure_event == 0 & scoring_data_strip$t <= 36
#                                            & scoring_data_strip$TRAINING == 1),]
# 
# fit_retire <- glm(retired_event ~    t_csb_8 +  t_csb_22 +  t_csb_35 +
#                     HGST + TOSHIBA + WEST_DIGIT + LT_1TB + LT_3TB + LT_5TB + LT_9TB + LT_12TB + 
#                     YR_2016 + YR_2017 + YR_2018 + smart_1_normalized, data=input_retire, family='binomial')
# 
# summary(fit_retire)
# 
# rm(input_retire)
# 
# scoring_data_strip$p_retire <- predict(fit_retire, newdata=scoring_data_strip, type='response')
# rm(fit_retire)
# 
# DBI::dbWriteTable(db,"SCORES_ALL",scoring_data_strip, overwrite = T, temporary = F)
# 
# rm(scoring_data_strip)
