eye_agreement <-
function(data, threshold, xRes, yRes) {
  
  data$Validity <- rep(NA, nrow(data))
  
  for (i in 1:nrow(data)) {
    if (!is.na(data$ValidityLeft[i])) {
      # need to convert to pixel measure to get distance
      dist <- sqrt((xRes*(data$GazePointXLeft[i] - data$GazePointXRight[i]))^2 + (yRes*(data$GazePointYLeft[i] - data$GazePointYRight[i]))^2)
      
      if (dist > threshold) {
        data$Validity[i] <- NA
      } else {
        data$Validity[i] <- data$ValidityLeft[i]
      }    
    }   
  }
  
  return(data)
  
}
