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
