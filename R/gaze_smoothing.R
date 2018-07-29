gaze_smoothing <-
function(data, windowSize, method) {
  
  data$GazePointXSmoothed <- rep(NA, nrow(data))
  data$GazePointYSmoothed <- rep(NA, nrow(data))
  movingAverageVectorX <- c()
  movingAverageVectorY <- c()  
  
  movingAverageHelper <- function(method, numbers){
    if (method == 'mean') {
      mean(numbers)
    } else {
      median(numbers)
    }
  }
  
  
  for (i in 1:nrow(data)) {
    
    # only one check should be necessary with the tracker used
    if ((!is.na(data$GazePointX[i])) & (!is.na(data$GazePointY[i])) & (!is.na(data$Validity[i]))) {
      
      
      # add number to window only if validity is true (not na)
      for (j in (-(windowSize-1)/2):((windowSize-1)/2)) {
        if ((i + j >= 1) & (i + j <= nrow(data))) {
          if (!is.na(data$Validity[i+j])) {
            movingAverageVectorX <- append(movingAverageVectorX, data$GazePointX[i+j])
            movingAverageVectorY <- append(movingAverageVectorY, data$GazePointY[i+j])              
          }
        }        
      }
      
      
      data$GazePointXSmoothed[i] <- movingAverageHelper(method, movingAverageVectorX)
      data$GazePointYSmoothed[i] <- movingAverageHelper(method, movingAverageVectorY)
      
      movingAverageVectorX <- c()
      movingAverageVectorY <- c()
      
    }    
  }  
  return(data)
}
