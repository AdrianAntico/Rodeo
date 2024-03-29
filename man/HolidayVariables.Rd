% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FeatureEngineering_CalendarTypes.R
\name{HolidayVariables}
\alias{HolidayVariables}
\title{HolidayVariables}
\usage{
HolidayVariables(
  data = NULL,
  RunMode = "train",
  ArgsList = ArgsList,
  SkipCols = NULL
)
}
\arguments{
\item{data}{Source data}

\item{RunMode}{'train' or 'score'}

\item{ArgsList}{ArgsList_FFE}

\item{SkipCols}{Vector of column names to remove from data}
}
\value{
A list containing the data and the ArgsList
}
\description{
Create holiday variables
}
\examples{
\dontrun{
Output <- AutoQuant:::HolidayVariables(
  data = data,
  RunMode = "train",
  ArgsList = ArgsList,
  SkipCols = NULL)
data <- Output$data
ArgsList_FE <- Output$ArgsList
}

}
\seealso{
Other Feature Engineering - Date Types: 
\code{\link{CalendarVariables}()}
}
\author{
Adrian Antico
}
\concept{Feature Engineering - Date Types}
