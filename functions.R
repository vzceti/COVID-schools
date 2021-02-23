requireLibs <- function(libs) {
  for (lib in libs){
    if(!is.element(lib, .packages(all.available = TRUE))) {
      install.packages(lib)
    }
    library(lib, character.only = TRUE)
  }
}

setthestatus <- function(x) {
  sapply(x, function(x) 
    if(x == "Fully Remote") 1 
    else if (x == "Partial Hybrid") 2
    else if (x == "All Hybrid") 3
    else if (x == "Partial in Person") 4
    else if (x == "In Person") 5
    else "NA")
}


knotsAndSpikes <- function(t_start, t_end, knots, spikes){
  a <<- Sys.time()
  
  # Define knots ----
  name_knots <- paste("t","csb",knots, sep="_")
  name_knots <- c("t", "t_sq", name_knots)
  time <- seq(t_start,t_end,by=1)
  time_sq <-  time^2
  csb_values<-data.frame()
  
  for(t in t_start:t_end){
    for(n in 1:length(knots)){
      csb_values[t+1,n]=(t>knots[n])*(t-knots[n])^3 - t^3 + 3*knots[n]*t^2 - 3*knots[n]^2*t
    }
  }
  
  csb_values <- cbind(time,time_sq,csb_values)
  colnames(csb_values) <- name_knots
  
  # Create spikes ----
  name_spk <- paste("t","spk",spikes, sep="_")
  spk_values <- data.frame()
  
  for(t in t_start:t_end){
    for(n in 1:length(spikes)){
      spk_values[t+1,n]=0
      if(t == spikes[n]){
        spk_values[t+1,n]=1
      }
    }
  }
  colnames(spk_values) <- name_spk
  finalDf <- cbind(csb_values, spk_values)
  finalDf$t <- as.integer(finalDf$t)
  rm(csb_values, spk_values)
  gc()
  return(finalDf)
}

renameknots <- function(input){
  paste("t_csb_", input, sep="")
}

renamespikes <- function(input){
  paste("t_spk_", input, sep="")
}

rankfunc <- function(data, years) {
  p <- Event_Modeling_Rank(
    input = data,
    models = "logit",
    train_year = years,
    Target.name = Target.name,
    feature.names = feature.names
  )
  return(p)
}


Event_Modeling_Rank <- function(input, models, train_year, Target.name, feature.names) {
  if ("logit" == models) {
    input_train <- input[lubridate::year(input$open_date) == training_period_start | lubridate::year(input$open_date) == training_period_end ,]
    # Logistic Regression # Modeling
    logit_18 <- glm(data = input_train,na.action = na.omit,
                    as.formula(paste(paste(Target.name, "~ "),paste(feature.names, collapse = " + "),sep = "")),
                    family = "binomial"
    )
    logit_18_step <- stepAIC(logit_18 , direction = "forward")
    
    print(summary(logit_18_step))
    # Predicting
    predict_logit <-logistic(predict(logit_18_step, newdata = input))
    return(predict_logit)
  }
} 


deciles <- function(data) {
  print(head(data_engineered))
  data <- as.data.table(data)
  for(i in seq(0.01,0.10,by = 0.01)){
    data[pred_logit >= i-0.01 & pred_logit < i , risk_grp := 100*i]
  }
  data[pred_logit >= 0.09 , risk_grp:= 10]}


twentiles <- function(data) {
  # This script uses the rank ordering model to get the risk twentiles and return a renamed p_co_18m
  #
  # args:
  #  data - the dataset with rank ordering model predictions
  #
  # returns:
  #  data - a new data frame with the predicted values and respective twentiles
  
  print(head(data_engineered))
  # Building Risk Twentiles ----
  data <- as.data.table(data)
  for(i in seq(0.01,0.19,by = 0.01)){
    data[pred_logit >= i-0.01 & pred_logit < i , risk_grp := 100*i]
  }
  data[pred_logit >= 0.19 , risk_grp:= 20]
  
  # rename pred_logit to p_co_18m ----  
  data <- data %>% 
    rename(!!p.rank.name:=pred_logit)
  data
}

deciles <- function(data) {
  # This script uses the rank ordering model to get the risk deciles and return a renamed p_co_18m
  #
  # args:
  #  data - the dataset with rank ordering model predictions
  #
  # returns:
  #  data - a new data frame with the predicted values and respective deciles
  
  print(head(data_engineered))
  # Building Risk Deciles ----
  data <- as.data.table(data)
  for(i in seq(0.01,0.10,by = 0.01)){
    data[pred_logit >= i-0.01 & pred_logit < i , risk_grp := 100*i]
  }
  data[pred_logit >= 0.09 , risk_grp:= 10]
  
  # rename pred_logit to p_co_18m ----  
  data <- data %>% 
    rename(!!p.rank.name:=pred_logit)
  data
}
