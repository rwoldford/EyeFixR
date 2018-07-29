make_fixation_table <-
function(data) {
  
  # produce a table describing fixations
  # FixationIndex Timestamp FixationDuration MappedFixationPointX MappedFixationPointY
  FixationIndex <- rep(0, max(data$fixationNumber[!is.na(data$fixationNumber)]))
  Timestamp <- rep(0, max(data$fixationNumber[!is.na(data$fixationNumber)]))
  FixationDuration <- rep(0, max(data$fixationNumber[!is.na(data$fixationNumber)]))
  MappedFixationPointX <- rep(0, max(data$fixationNumber[!is.na(data$fixationNumber)]))
  MappedFixationPointY <- rep(0, max(data$fixationNumber[!is.na(data$fixationNumber)]))
  
  # the first fixation may be numbered 2 or 1, so start at the minimum
  for (i in (min(data$fixationNumber[!is.na(data$fixationNumber)])):(max(data$fixationNumber[!is.na(data$fixationNumber)]))) {
    FixationIndex[i] <- i
    Timestamp[i] <- data$Timestamp[!is.na(data$fixationNumber) & data$fixationNumber == i][1]
    FixationDuration[i] <- tail(data$Timestamp[!is.na(data$fixationNumber) & data$fixationNumber == i], 1) - Timestamp[i]
    MappedFixationPointX[i] <- mean(data$GazePointX[!is.na(data$fixationNumber) & data$fixationNumber == i])
    MappedFixationPointY[i] <- mean(data$GazePointY[!is.na(data$fixationNumber) & data$fixationNumber == i])
  }
  
  fixations <- data.frame(FixationIndex, Timestamp, FixationDuration, MappedFixationPointX, MappedFixationPointY)
  
  return(fixations)
}
