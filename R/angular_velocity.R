angular_velocity <-
function(data, xDist, yDist) {
  
  # angular velocity - units of box size are mm
  data$AngularVelocity <- rep(NA, nrow(data))
  
  for (i in 2:nrow(data)) {
    if (!is.na(data$GazePointX[i]) & !is.na(data$GazePointX[i-1])) {
      timeDiff <- (data$Timestamp[i] - data$Timestamp[i-1])*10^(-6)
      x <- (data$EyeLocX[i] + data$EyeLocX[i-1])/2
      y <- (data$EyeLocY[i] + data$EyeLocY[i-1])/2
      z <- (data$EyeLocZ[i] + data$EyeLocZ[i-1])/2
      
      a <- sqrt(z^2 + (x - xDist*data$GazePointXSmoothed[i])^2 + (y - yDist*data$GazePointYSmoothed[i])^2)
      b <- sqrt(z^2 + (x - xDist*data$GazePointXSmoothed[i-1])^2 + (y - yDist*data$GazePointYSmoothed[i-1])^2)
      c <- sqrt((xDist*data$GazePointXSmoothed[i] - xDist*data$GazePointXSmoothed[i-1])^2 + (yDist*data$GazePointYSmoothed[i] - yDist*data$GazePointYSmoothed[i-1])^2)
      
      angle <- 180/pi*acos((a^2 + b^2 - c^2)/(2*a*b))
      
      data$AngularVelocity[i] <- angle/timeDiff
      
    }    
  }  
  return(data)
}
