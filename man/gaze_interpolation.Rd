\name{gaze_interpolation}
\alias{gaze_interpolation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{interpolate gaze points in small gaps}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gaze_interpolation(data, maxGapLength)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{maxGapLength}{
%%     ~~Describe \code{maxGapLength} here~~
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
function (data, maxGapLength) 
{
    lastValid <- 0
    for (i in 1:nrow(data)) {
        if (!is.na(data$Validity[i])) {
            lastValid <- i
        }
        else if ((i < length(data$Validity)) & (!is.na(data$Validity[i + 
            1])) & (lastValid > 0)) {
            if ((data$Timestamp[i + 1] - data$Timestamp[lastValid] <= 
                maxGapLength)) {
                for (j in (lastValid + 1):i) {
                  s <- (j - lastValid)/((i + 1) - lastValid)
                  t <- data$GazePointX[i + 1] - data$GazePointX[lastValid]
                  t2 <- data$GazePointY[i + 1] - data$GazePointY[lastValid]
                  data$GazePointX[j] <- s * t + data$GazePointX[lastValid]
                  data$GazePointY[j] <- s * t2 + data$GazePointY[lastValid]
                  t <- data$EyeLocX[i + 1] - data$EyeLocX[lastValid]
                  t2 <- data$EyeLocY[i + 1] - data$EyeLocY[lastValid]
                  t3 <- data$EyeLocZ[i + 1] - data$EyeLocZ[lastValid]
                  data$EyeLocX[j] <- s * t + data$EyeLocX[lastValid]
                  data$EyeLocY[j] <- s * t2 + data$EyeLocY[lastValid]
                  data$EyeLocZ[j] <- s * t3 + data$EyeLocZ[lastValid]
                  t <- data$Timestamp[i + 1] - data$Timestamp[lastValid]
                  data$Timestamp[j] <- s * t + data$Timestamp[lastValid]
                  data$Validity[i] <- 0
                }
            }
        }
    }
    return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
