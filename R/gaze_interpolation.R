gaze_interpolation <-
function(data, maxGapLength) {
  
  lastValid <- 0;
  
  for (i in 1:nrow(data)) {
    
    # if the current row is valid, update lastValid
    if (!is.na(data$Validity[i])) {
      lastValid <- i
      
      # if the current row is invalid and the next is valid, we have reached the end of a gap
    } else if ( (i < length(data$Validity)) & (!is.na(data$Validity[i+1])) & (lastValid > 0 ))  {  
      
      
      if ((data$Timestamp[i+1] - data$Timestamp[lastValid] <= maxGapLength)) { #*** change to i+1 from i, may need to change range
        # iterate over the length of the gap
        # *** ignoring written equation in paper and going with the written explanation here
        for (j in (lastValid + 1):i) {
          s <- (j - lastValid)/((i + 1) - lastValid);
          
          t <- data$GazePointX[i + 1] - data$GazePointX[lastValid];
          t2 <- data$GazePointY[i + 1] - data$GazePointY[lastValid];
          data$GazePointX[j] <- s*t + data$GazePointX[lastValid];
          data$GazePointY[j] <- s*t2 + data$GazePointY[lastValid];       
          
          t <- data$EyeLocX[i + 1] - data$EyeLocX[lastValid];
          t2 <- data$EyeLocY[i + 1] - data$EyeLocY[lastValid];
          t3 <- data$EyeLocZ[i + 1] - data$EyeLocZ[lastValid];
          data$EyeLocX[j] <- s*t + data$EyeLocX[lastValid];
          data$EyeLocY[j] <- s*t2 + data$EyeLocY[lastValid];
          data$EyeLocZ[j] <- s*t3 + data$EyeLocZ[lastValid];
          
          t <- data$Timestamp[i+1] - data$Timestamp[lastValid];
          data$Timestamp[j] <- s*t + data$Timestamp[lastValid];
          
          data$Validity[i] <- 0;
          
          
        }
      } 
      # otherwise we are somewhere in the middle of a gap, do nothing
    }    
  }
  
  return(data)
}
