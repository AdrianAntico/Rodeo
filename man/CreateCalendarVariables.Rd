% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FeatureEngineering_CalendarTypes.R
\name{CreateCalendarVariables}
\alias{CreateCalendarVariables}
\title{CreateCalendarVariables}
\usage{
CreateCalendarVariables(
  data,
  DateCols = NULL,
  AsFactor = FALSE,
  TimeUnits = "wday",
  CachePath = NULL,
  Debug = FALSE
)
}
\arguments{
\item{data}{This is your data}

\item{DateCols}{Supply either column names or column numbers of your date columns you want to use for creating calendar variables}

\item{AsFactor}{Set to TRUE if you want factor type columns returned; otherwise integer type columns will be returned}

\item{TimeUnits}{Supply a character vector of time units for creating calendar variables. Options include: "second", "minute", "hour", "wday", "mday", "yday", "week", "isoweek", "wom" (week of month), "month", "quarter", "year"}

\item{CachePath}{Path to data in a local directory. .csv only for now}

\item{Debug}{= FALSE}
}
\value{
Returns your data.table with the added calendar variables at the end
}
\description{
CreateCalendarVariables Rapidly creates calendar variables based on the date column you provide
}
\examples{
\dontrun{
# Create fake data with a Date column----
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.75,
  N = 25000L,
  ID = 2L,
  ZIP = 0L,
  FactorCount = 4L,
  AddDate = TRUE,
  Classification = FALSE,
  MultiClass = FALSE)
for(i in seq_len(20L)) {
  print(i)
  data <- data.table::rbindlist(
    list(data, AutoQuant::FakeDataGenerator(
    Correlation = 0.75,
    N = 25000L,
    ID = 2L,
    ZIP = 0L,
    FactorCount = 4L,
    AddDate = TRUE,
    Classification = FALSE,
    MultiClass = FALSE)))
}

# Create calendar variables - automatically excludes
#   the second, minute, and hour selections since
#   it is not timestamp data
runtime <- system.time(
  data <- AutoQuant::CreateCalendarVariables(
    data = data,
    DateCols = "DateTime",
    AsFactor = FALSE,
    TimeUnits = c("second",
                  "minute",
                  "hour",
                  "wday",
                  "mday",
                  "yday",
                  "week",
                  "isoweek",
                  "wom",
                  "month",
                  "quarter",
                  "year")))
head(data)
print(runtime)
}
}
\seealso{
Other Feature Engineering: 
\code{\link{Apply_Asinh}()},
\code{\link{Apply_Asin}()},
\code{\link{Apply_BoxCox}()},
\code{\link{Apply_LogPlus1}()},
\code{\link{Apply_Logit}()},
\code{\link{Apply_Log}()},
\code{\link{Apply_Sqrt}()},
\code{\link{Apply_YeoJohnson}()},
\code{\link{AutoDataPartition}()},
\code{\link{AutoDiffLagN}()},
\code{\link{AutoInteraction}()},
\code{\link{AutoLagRollMode}()},
\code{\link{AutoLagRollStatsScoring}()},
\code{\link{AutoLagRollStats}()},
\code{\link{AutoTransformationCreate}()},
\code{\link{AutoTransformationScore}()},
\code{\link{AutoWord2VecModeler}()},
\code{\link{AutoWord2VecScoring}()},
\code{\link{CategoricalEncoding}()},
\code{\link{CreateHolidayVariables}()},
\code{\link{DT_GDL_Feature_Engineering}()},
\code{\link{DummifyDT}()},
\code{\link{Estimate_BoxCox_Lambda}()},
\code{\link{Estimate_YeoJohnson_Lambda}()},
\code{\link{H2OAutoencoderScoring}()},
\code{\link{H2OAutoencoder}()},
\code{\link{Interact}()},
\code{\link{InvApply_Asinh}()},
\code{\link{InvApply_Asin}()},
\code{\link{InvApply_BoxCox}()},
\code{\link{InvApply_LogPlus1}()},
\code{\link{InvApply_Logit}()},
\code{\link{InvApply_Log}()},
\code{\link{InvApply_Sqrt}()},
\code{\link{InvApply_YeoJohnson}()},
\code{\link{ModelDataPrep}()},
\code{\link{Partial_DT_GDL_Feature_Engineering2}()},
\code{\link{Partial_DT_GDL_Feature_Engineering}()},
\code{\link{PercRankScoring}()},
\code{\link{PercRank}()},
\code{\link{StandardizeScoring}()},
\code{\link{Standardize}()},
\code{\link{Test_Asinh}()},
\code{\link{Test_Asin}()},
\code{\link{Test_BoxCox}()},
\code{\link{Test_Identity}()},
\code{\link{Test_LogPlus1}()},
\code{\link{Test_Logit}()},
\code{\link{Test_Log}()},
\code{\link{Test_Sqrt}()},
\code{\link{Test_YeoJohnson}()},
\code{\link{TimeSeriesFillRoll}()},
\code{\link{TimeSeriesFill}()}
}
\author{
Adrian Antico
}
\concept{Feature Engineering}
