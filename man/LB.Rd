% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FeatureEngineering_CalendarTypes.R
\name{LB}
\alias{LB}
\title{LB}
\usage{
LB(TimeAgg)
}
\arguments{
\item{TimeAgg}{Valid options are "hour", "hours", "1min", "1mins", "1minute", "1minutes", "5min", "5mins", "5minute", "5minutes","10min", "10mins", "10minute", "10minutes", "15min", "15mins", "15minute", "15minutes", "30min", "30mins", "30minute", "30minutes", "day", "days", "week", "weeks", "month", "months", "quarter", "quarters", "years", "year"}
}
\description{
Create default for CreateHolidayVariables
}
\examples{
\dontrun{
Lookback <- LB("days")
}

}
\seealso{
Other Misc: 
\code{\link{DiffDT}()}
}
\author{
Adrian Antico
}
\concept{Misc}
