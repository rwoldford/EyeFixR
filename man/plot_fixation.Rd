\name{plot_fixation}
\alias{plot_fixation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{make a plot of fixations}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot_fixation(data, fixations, durationLimit = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{fixations}{
%%     ~~Describe \code{fixations} here~~
}
  \item{durationLimit}{
%%     ~~Describe \code{durationLimit} here~~
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
function (data, fixations, durationLimit = 0) 
{
    par(mar = c(0, 0, 0, 0))
    plot(data$GazePointX[!is.na(data$GazePointX)], 1 - data$GazePointY[!is.na(data$GazePointY)], 
        cex = 0.1, col = "red", xlim = c(0, 1), ylim = c(0, 1))
    lines(data$GazePointX[!is.na(data$GazePointX)], 1 - data$GazePointY[!is.na(data$GazePointY)], 
        col = "red")
    points(data$GazePointX[1], 1 - data$GazePointY[1], col = "green", 
        cex = 3, pch = 20)
    for (i in 1:nrow(fixations)) {
        if ((fixations$FixationIndex[i] >= 1) & (fixations$FixationDuration[i] > 
            durationLimit)) {
            pointSize <- 5 * fixations$FixationDuration[i]/max(fixations$FixationDuration)
            points(fixations$MappedFixationPointX[i], 1 - fixations$MappedFixationPointY[i], 
                col = "blue", cex = pointSize, pch = 1)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
