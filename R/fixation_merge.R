fixation_merge <-
function(fix1, fix2, fixations, data) {
  #Index <- fix1
  Starttime <- fixations$Timestamp[FixationIndex == fix1]
  Duration <- tail(data$Timestamp[!is.na(data$fixationNumber) & data$fixationNumber == fix2], 1) - Starttime
  FixationPointX <- mean(data$GazePointX[!is.na(data$fixationNumber) & (data$fixationNumber == fix1 | data$fixationNumber == fix2)])
  FixationPointY <- mean(data$GazePointY[!is.na(data$fixationNumber) & (data$fixationNumber == fix1 | data$fixationNumber == fix2)])
  
  fixations$FixationDuration[fixations$FixationIndex == fix1] <- Duration
  fixations$MappedFixationPointX[fixations$FixationIndex == fix1] <- FixationPointX
  fixations$MappedFixationPointY[fixations$FixationIndex == fix1] <- FixationPointY
  
  fixations <- fixations[!(fixations$FixationIndex == fix2),]
  
  return(fixations)
  
}
