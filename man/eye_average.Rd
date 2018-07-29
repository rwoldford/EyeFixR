\name{eye_average}
\alias{eye_average}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{average left and right eye gaze points}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eye_average(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function (data) 
{
    data$GazePointX <- rep(NA, nrow(data))
    data$GazePointY <- rep(NA, nrow(data))
    data$EyeLocX <- rep(NA, nrow(data))
    data$EyeLocY <- rep(NA, nrow(data))
    data$EyeLocZ <- rep(NA, nrow(data))
    for (i in 1:nrow(data)) {
        if (!is.na(data$Validity[i])) {
            data$GazePointX[i] <- (data$GazePointXLeft[i] + data$GazePointXRight[i])/2
            data$GazePointY[i] <- (data$GazePointYLeft[i] + data$GazePointYRight[i])/2
            data$EyeLocX[i] <- (data$EyeLocXLeft[i] + data$EyeLocXRight[i])/2
            data$EyeLocY[i] <- (data$EyeLocYLeft[i] + data$EyeLocYRight[i])/2
            data$EyeLocZ[i] <- (data$EyeLocZLeft[i] + data$EyeLocZRight[i])/2
        }
    }
    return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
