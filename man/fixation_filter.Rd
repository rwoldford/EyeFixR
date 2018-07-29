\name{fixation_filter}
\alias{fixation_filter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{pick out fixations from data}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fixation_filter(file, xResolution = 1280, yResolution = 1024, xMM = 340, yMM = 270, agreementThreshold = 200, maxInterpolation = 75000, smoothingWindowSize = 3, smoothingMethod = "median", fixationMaxVelocity = 30)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{xResolution}{
%%     ~~Describe \code{xResolution} here~~
}
  \item{yResolution}{
%%     ~~Describe \code{yResolution} here~~
}
  \item{xMM}{
%%     ~~Describe \code{xMM} here~~
}
  \item{yMM}{
%%     ~~Describe \code{yMM} here~~
}
  \item{agreementThreshold}{
%%     ~~Describe \code{agreementThreshold} here~~
}
  \item{maxInterpolation}{
%%     ~~Describe \code{maxInterpolation} here~~
}
  \item{smoothingWindowSize}{
%%     ~~Describe \code{smoothingWindowSize} here~~
}
  \item{smoothingMethod}{
%%     ~~Describe \code{smoothingMethod} here~~
}
  \item{fixationMaxVelocity}{
%%     ~~Describe \code{fixationMaxVelocity} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (file, xResolution = 1280, yResolution = 1024, xMM = 340, 
    yMM = 270, agreementThreshold = 200, maxInterpolation = 75000, 
    smoothingWindowSize = 3, smoothingMethod = "median", fixationMaxVelocity = 30) 
{
    data <- read.table(file, sep = ",", header = FALSE, strip.white = TRUE)
    names(data) <- c("Timestamp", "ValidityLeft", "EyeLocXLeft", 
        "EyeLocYLeft", "EyeLocZLeft", "GazePointXLeft", "GazePointYLeft", 
        "ValidityRight", "EyeLocXRight", "EyeLocYRight", "EyeLocZRight", 
        "GazePointXRight", "GazePointYRight")
    data <- eye_agreement(data, agreementThreshold, xResolution, 
        yResolution)
    data <- eye_average(data)
    data <- gaze_interpolation(data, maxInterpolation)
    data <- gaze_smoothing(data, smoothingWindowSize, smoothingMethod)
    data <- angular_velocity(data, xMM, yMM)
    data <- fixation_classifier(data, fixationMaxVelocity)
    return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
