\name{make_fixation_table}
\alias{make_fixation_table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{put fixation results in a table}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make_fixation_table(data)
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
    FixationIndex <- rep(0, max(data$fixationNumber[!is.na(data$fixationNumber)]))
    Timestamp <- rep(0, max(data$fixationNumber[!is.na(data$fixationNumber)]))
    FixationDuration <- rep(0, max(data$fixationNumber[!is.na(data$fixationNumber)]))
    MappedFixationPointX <- rep(0, max(data$fixationNumber[!is.na(data$fixationNumber)]))
    MappedFixationPointY <- rep(0, max(data$fixationNumber[!is.na(data$fixationNumber)]))
    for (i in (min(data$fixationNumber[!is.na(data$fixationNumber)])):(max(data$fixationNumber[!is.na(data$fixationNumber)]))) {
        FixationIndex[i] <- i
        Timestamp[i] <- data$Timestamp[!is.na(data$fixationNumber) & 
            data$fixationNumber == i][1]
        FixationDuration[i] <- tail(data$Timestamp[!is.na(data$fixationNumber) & 
            data$fixationNumber == i], 1) - Timestamp[i]
        MappedFixationPointX[i] <- mean(data$GazePointX[!is.na(data$fixationNumber) & 
            data$fixationNumber == i])
        MappedFixationPointY[i] <- mean(data$GazePointY[!is.na(data$fixationNumber) & 
            data$fixationNumber == i])
    }
    fixations <- data.frame(FixationIndex, Timestamp, FixationDuration, 
        MappedFixationPointX, MappedFixationPointY)
    return(fixations)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
