eye_average <-
function(data) {
  data$GazePointX <- rep(NA, nrow(data))
  data$GazePointY <- rep(NA, nrow(data))
  data$EyeLocX <- rep(NA, nrow(data))
  data$EyeLocY <- rep(NA, nrow(data))
  data$EyeLocZ <- rep(NA, nrow(data))
  
  for (i in 1:nrow(data)) {
    if (!is.na(data$Validity[i])) {
      data$GazePointX[i] <- (data$GazePointXLeft[i] + data$GazePointXRight[i])/2
      data$GazePointY[i] <- (data$GazePointYLeft[i] + data$GazePointYRight[i])/2
      data$EyeLocX[i] <- (data$EyeLocXLeft[i] + data$EyeLocXRight[i])/2
      data$EyeLocY[i] <- (data$EyeLocYLeft[i] + data$EyeLocYRight[i])/2
      data$EyeLocZ[i] <- (data$EyeLocZLeft[i] + data$EyeLocZRight[i])/2 
      
    }   
  }  
  return(data)  
}
