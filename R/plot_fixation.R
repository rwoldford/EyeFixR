plot_fixation <-
function(data, fixations, durationLimit = 0) {
  
  par(mar = c(0,0,0,0))
  # consider removing the is.na check here, to show gaps on the image as well
  plot(data$GazePointX[!is.na(data$GazePointX)], 1-data$GazePointY[!is.na(data$GazePointY)], cex = 0.1, col = 'red', xlim = c(0,1), ylim = c(0,1))
  lines(data$GazePointX[!is.na(data$GazePointX)], 1-data$GazePointY[!is.na(data$GazePointY)], col = 'red')
  points(data$GazePointX[1], 1-data$GazePointY[1], col = 'green', cex = 3, pch = 20)
  for (i in 1:nrow(fixations)) {
    if((fixations$FixationIndex[i] >= 1) & (fixations$FixationDuration[i] > durationLimit)) {
      
      pointSize <- 5*fixations$FixationDuration[i]/max(fixations$FixationDuration)
      points(fixations$MappedFixationPointX[i], 1-fixations$MappedFixationPointY[i], col = 'blue', cex = pointSize, pch = 1)
    }
  }  
}
