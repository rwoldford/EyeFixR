\name{fixation_merge}
\alias{fixation_merge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{merge fixations that are decided to be too close}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fixation_merge(fix1, fix2, fixations, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fix1}{
%%     ~~Describe \code{fix1} here~~
}
  \item{fix2}{
%%     ~~Describe \code{fix2} here~~
}
  \item{fixations}{
%%     ~~Describe \code{fixations} here~~
}
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
function (fix1, fix2, fixations, data) 
{
    Starttime <- fixations$Timestamp[FixationIndex == fix1]
    Duration <- tail(data$Timestamp[!is.na(data$fixationNumber) & 
        data$fixationNumber == fix2], 1) - Starttime
    FixationPointX <- mean(data$GazePointX[!is.na(data$fixationNumber) & 
        (data$fixationNumber == fix1 | data$fixationNumber == 
            fix2)])
    FixationPointY <- mean(data$GazePointY[!is.na(data$fixationNumber) & 
        (data$fixationNumber == fix1 | data$fixationNumber == 
            fix2)])
    fixations$FixationDuration[fixations$FixationIndex == fix1] <- Duration
    fixations$MappedFixationPointX[fixations$FixationIndex == 
        fix1] <- FixationPointX
    fixations$MappedFixationPointY[fixations$FixationIndex == 
        fix1] <- FixationPointY
    fixations <- fixations[!(fixations$FixationIndex == fix2), 
        ]
    return(fixations)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
