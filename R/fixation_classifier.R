fixation_classifier <-
function(data, degPerSec) {
  
  data$fixation <- rep(NA, nrow(data))  
  
  for (i in 1:nrow(data)) {
    if(!is.na(data$AngularVelocity[i]) & data$AngularVelocity[i] >= degPerSec) {
      data$fixation[i] <- FALSE
      
    } else if (!is.na(data$AngularVelocity[i])) {
      data$fixation[i] <- TRUE
      
    }
  }
  
  # number fixations
  currentNumber <- 1
  data$fixationNumber <- rep(NA, nrow(data))
  
  for ( i in 1: nrow(data)) {
    if(!is.na(data$fixation[i]) & data$fixation[i]) {
      data$fixationNumber[i] <- currentNumber
    } else if (!is.na(data$fixation[i+1]) & (i < nrow(data)) & (data$fixation[i+1] == TRUE)) {
      # increment the number when a fixation value of FALSE or NA has a next value of TRUE
      currentNumber = currentNumber + 1
    } 
  }  
  return(data)  
}
