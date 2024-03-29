% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FeatureEngineering_CrossRowOperations.R
\name{DT_GDL_Feature_Engineering}
\alias{DT_GDL_Feature_Engineering}
\title{DT_GDL_Feature_Engineering}
\usage{
DT_GDL_Feature_Engineering(
  data,
  lags = 1,
  periods = 0,
  SDperiods = 0,
  Skewperiods = 0,
  Kurtperiods = 0,
  Quantileperiods = 0,
  statsFUNs = c("mean"),
  targets = NULL,
  groupingVars = NULL,
  sortDateName = NULL,
  timeDiffTarget = NULL,
  timeAgg = c("days"),
  WindowingLag = 0,
  ShortName = TRUE,
  Type = c("Lag"),
  SimpleImpute = TRUE
)
}
\arguments{
\item{data}{A data.table you want to run the function on}

\item{lags}{A numeric vector of the specific lags you want to have generated. You must include 1 if WindowingLag = 1.}

\item{periods}{A numeric vector of the specific rolling statistics window sizes you want to utilize in the calculations.}

\item{SDperiods}{A numeric vector of Standard Deviation rolling statistics window sizes you want to utilize in the calculations.}

\item{Skewperiods}{A numeric vector of Skewness rolling statistics window sizes you want to utilize in the calculations.}

\item{Kurtperiods}{A numeric vector of Kurtosis rolling statistics window sizes you want to utilize in the calculations.}

\item{Quantileperiods}{A numeric vector of Quantile rolling statistics window sizes you want to utilize in the calculations.}

\item{statsFUNs}{Select from the following c("mean","sd","skew","kurt","q5","q10","q15","q20","q25","q30","q35","q40","q45","q50","q55","q60","q65","q70","q75","q80","q85","q90","q95")}

\item{targets}{A character vector of the column names for the reference column in which you will build your lags and rolling stats}

\item{groupingVars}{A character vector of categorical variable names you will build your lags and rolling stats by}

\item{sortDateName}{The column name of your date column used to sort events over time}

\item{timeDiffTarget}{Specify a desired name for features created for time between events. Set to NULL if you don't want time between events features created.}

\item{timeAgg}{List the time aggregation level for the time between events features, such as "hour", "day", "week", "month", "quarter", or "year"}

\item{WindowingLag}{Set to 0 to build rolling stats off of target columns directly or set to 1 to build the rolling stats off of the lag-1 target}

\item{ShortName}{Default TRUE. If FALSE, Group Variable names will be added to the rolling stat and lag names. If you plan on have multiple versions of lags and rollings stats by different group variables then set this to FALSE.}

\item{Type}{List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values}

\item{SimpleImpute}{Set to TRUE for factor level imputation of "0" and numeric imputation of -1}
}
\value{
data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
}
\description{
Builds autoregressive and moving average from target columns and distributed lags and distributed moving average for independent features distributed across time. On top of that, you can also create time between instances along with their associated lags and moving averages. This function works for data with groups and without groups.
}
\examples{
\dontrun{
N = 25116
data <- data.table::data.table(
  DateTime = as.Date(Sys.time()),
  Target = stats::filter(rnorm(N, mean = 50, sd = 20),
  filter=rep(1,10),
  circular=TRUE))
data[, temp := seq(1:N)][, DateTime := DateTime - temp][
  , temp := NULL]
data <- data[order(DateTime)]
data <- DT_GDL_Feature_Engineering(
  data,
  lags           = c(seq(1,5,1)),
  periods        = c(3,5,10,15,20,25),
  SDperiods       = c(seq(5, 95, 5)),
  Skewperiods     = c(seq(5, 95, 5)),
  Kurtperiods     = c(seq(5, 95, 5)),
  Quantileperiods = c(seq(5, 95, 5)),
  Modeperiods     = 0,
  statsFUNs      = c("mean",
    "sd","skew","kurt","q05","q95"),
  targets        = c("Target"),
  groupingVars   = NULL,
  sortDateName   = "DateTime",
  timeDiffTarget = NULL, # deprecated
  timeAgg        = c("days"),
  WindowingLag   = 1,
  Type           = "Lag",
  SimpleImpute   = TRUE)
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
\code{\link{CreateCalendarVariables}()},
\code{\link{CreateHolidayVariables}()},
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
