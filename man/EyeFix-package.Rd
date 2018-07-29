\name{EyeFix-package}
\alias{EyeFix-package}
\alias{EyeFix}
\docType{package}
\title{Analysis of eye tracking data}
\description{This package is an implementation of the fixation filter algorithm as described in http://www.tobii.com/eye-tracking-research/global/library/white-papers/the-tobii-i-vt-fixation-filter/.
The functions included break down the gaze point and eye position information output from a Tobii eye tracker, in order to identify fixation locations and durations, and plot the fixations on a scaled plot. }
\details{
\tabular{ll}{
Package: \tab EyeFix\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-05-20\cr
License: \tab What license is it under?\cr
}
The fixation_filter function will output the original data with additional information. 
Following that, make_fixation_table takes the data and produces a table with fixations numbered and with duration and location information. 
The fixation table can then lead to plotting the fixations, using the plot_fixation function. }
\author{Amanda Murdoch}
\references{\description{ http://www.tobii.com/eye-tracking-research/global/library/white-papers/the-tobii-i-vt-fixation-filter/}
\keyword{ package }
\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{

data("sample_gaze_data")
data <- fixation_filter(sample_gaze_data)
fixations <- make_fixation_table(data)
plot_fixation(data, fixations)
}
