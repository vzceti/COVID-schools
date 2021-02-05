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