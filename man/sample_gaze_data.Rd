\name{sample_gaze_data}
\alias{sample_gaze_data}
\docType{data}
\title{table with output from tobii eye tracker}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("sample_gaze_data")}
\format{
  A data frame with 347 observations on the following 24 variables.
  \describe{
    \item{\code{Timestamp}}{a numeric vector}
    \item{\code{ValidityLeft}}{a numeric vector}
    \item{\code{EyeLocXLeft}}{a numeric vector}
    \item{\code{EyeLocYLeft}}{a numeric vector}
    \item{\code{EyeLocZLeft}}{a numeric vector}
    \item{\code{GazePointXLeft}}{a numeric vector}
    \item{\code{GazePointYLeft}}{a numeric vector}
    \item{\code{ValidityRight}}{a numeric vector}
    \item{\code{EyeLocXRight}}{a numeric vector}
    \item{\code{EyeLocYRight}}{a numeric vector}
    \item{\code{EyeLocZRight}}{a numeric vector}
    \item{\code{GazePointXRight}}{a numeric vector}
    \item{\code{GazePointYRight}}{a numeric vector}
    \item{\code{Validity}}{a numeric vector}
    \item{\code{GazePointX}}{a numeric vector}
    \item{\code{GazePointY}}{a numeric vector}
    \item{\code{EyeLocX}}{a numeric vector}
    \item{\code{EyeLocY}}{a numeric vector}
    \item{\code{EyeLocZ}}{a numeric vector}
    \item{\code{GazePointXSmoothed}}{a numeric vector}
    \item{\code{GazePointYSmoothed}}{a numeric vector}
    \item{\code{AngularVelocity}}{a numeric vector}
    \item{\code{fixation}}{a logical vector}
    \item{\code{fixationNumber}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(sample_gaze_data)
## maybe str(sample_gaze_data) ; plot(sample_gaze_data) ...
}
\keyword{datasets}
