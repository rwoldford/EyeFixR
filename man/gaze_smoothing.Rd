\name{gaze_smoothing}
\alias{gaze_smoothing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{smooth gaze points}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gaze_smoothing(data, windowSize, method)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{windowSize}{
%%     ~~Describe \code{windowSize} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
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
function (data, windowSize, method) 
{
    data$GazePointXSmoothed <- rep(NA, nrow(data))
    data$GazePointYSmoothed <- rep(NA, nrow(data))
    movingAverageVectorX <- c()
    movingAverageVectorY <- c()
    movingAverageHelper <- function(method, numbers) {
        if (method == "mean") {
            mean(numbers)
        }
        else {
            median(numbers)
        }
    }
    for (i in 1:nrow(data)) {
        if ((!is.na(data$GazePointX[i])) & (!is.na(data$GazePointY[i])) & 
            (!is.na(data$Validity[i]))) {
            for (j in (-(windowSize - 1)/2):((windowSize - 1)/2)) {
                if ((i + j >= 1) & (i + j <= nrow(data))) {
                  if (!is.na(data$Validity[i + j])) {
                    movingAverageVectorX <- append(movingAverageVectorX, 
                      data$GazePointX[i + j])
                    movingAverageVectorY <- append(movingAverageVectorY, 
                      data$GazePointY[i + j])
                  }
                }
            }
            data$GazePointXSmoothed[i] <- movingAverageHelper(method, 
                movingAverageVectorX)
            data$GazePointYSmoothed[i] <- movingAverageHelper(method, 
                movingAverageVectorY)
            movingAverageVectorX <- c()
            movingAverageVectorY <- c()
        }
    }
    return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
