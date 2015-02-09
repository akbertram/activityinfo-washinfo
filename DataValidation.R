## Version 0.1 date: 20141012
## Version 0.2 date: 20141112
# Author: Carlos Velez email:carlos_velez@yahoo.com

DataValidation <- function(x, y, ValidValues){

  for (i in 1:length(x)){
    if (is.element(x[i], ValidValues)){
      y[i] <- "v"
    } else {
      x[i] <- NA      
      y[i] <- "i"     
    }
  }
  output <-list(x,y)
  return(output) 
}