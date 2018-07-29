fixation_filter <-
function(data, xResolution = 1280, yResolution = 1024, xMM = 340, yMM = 270, agreementThreshold = 200, maxInterpolation = 75000, smoothingWindowSize = 3, smoothingMethod = 'median', fixationMaxVelocity = 30) {
  
  #data <- read.table(file, sep = ',', header = FALSE, strip.white = TRUE)  
  names(data) <- c('Timestamp', 'ValidityLeft', 'EyeLocXLeft', 'EyeLocYLeft', 'EyeLocZLeft', 'GazePointXLeft', 'GazePointYLeft', 'ValidityRight', 'EyeLocXRight', 'EyeLocYRight', 'EyeLocZRight', 'GazePointXRight', 'GazePointYRight')
  
  data <- eye_agreement(data, agreementThreshold, xResolution, yResolution)  
  data <- eye_average(data)  
  data <- gaze_interpolation(data, maxInterpolation)  
  data <- gaze_smoothing(data, smoothingWindowSize, smoothingMethod)  
  data <- angular_velocity(data, xMM, yMM)  
  data <- fixation_classifier(data, fixationMaxVelocity)
  
  return(data)
}
