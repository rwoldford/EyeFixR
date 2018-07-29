\name{eye_agreement}
\alias{eye_agreement}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{check if left and right eyes agree}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eye_agreement(data, threshold, xRes, yRes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
}
  \item{xRes}{
%%     ~~Describe \code{xRes} here~~
}
  \item{yRes}{
%%     ~~Describe \code{yRes} here~~
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
function (data, threshold, xRes, yRes) 
{
    data$Validity <- rep(NA, nrow(data))
    for (i in 1:nrow(data)) {
        if (!is.na(data$ValidityLeft[i])) {
            dist <- sqrt((xRes * (data$GazePointXLeft[i] - data$GazePointXRight[i]))^2 + 
                (yRes * (data$GazePointYLeft[i] - data$GazePointYRight[i]))^2)
            if (dist > threshold) {
                data$Validity[i] <- NA
            }
            else {
                data$Validity[i] <- data$ValidityLeft[i]
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
