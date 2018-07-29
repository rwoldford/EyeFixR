\name{angular_velocity}
\alias{angular_velocity}
\title{angular velocity of eye movement}
\description{}
\usage{
angular_velocity(data, xDist, yDist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{xDist}{
%%     ~~Describe \code{xDist} here~~
}
  \item{yDist}{
%%     ~~Describe \code{yDist} here~~
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
function (data, xDist, yDist) 
{
    data$AngularVelocity <- rep(NA, nrow(data))
    for (i in 2:nrow(data)) {
        if (!is.na(data$GazePointX[i]) & !is.na(data$GazePointX[i - 
            1])) {
            timeDiff <- (data$Timestamp[i] - data$Timestamp[i - 
                1]) * 10^(-6)
            x <- (data$EyeLocX[i] + data$EyeLocX[i - 1])/2
            y <- (data$EyeLocY[i] + data$EyeLocY[i - 1])/2
            z <- (data$EyeLocZ[i] + data$EyeLocZ[i - 1])/2
            a <- sqrt(z^2 + (x - xDist * data$GazePointXSmoothed[i])^2 + 
                (y - yDist * data$GazePointYSmoothed[i])^2)
            b <- sqrt(z^2 + (x - xDist * data$GazePointXSmoothed[i - 
                1])^2 + (y - yDist * data$GazePointYSmoothed[i - 
                1])^2)
            c <- sqrt((xDist * data$GazePointXSmoothed[i] - xDist * 
                data$GazePointXSmoothed[i - 1])^2 + (yDist * 
                data$GazePointYSmoothed[i] - yDist * data$GazePointYSmoothed[i - 
                1])^2)
            angle <- 180/pi * acos((a^2 + b^2 - c^2)/(2 * a * 
                b))
            data$AngularVelocity[i] <- angle/timeDiff
        }
    }
    return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
