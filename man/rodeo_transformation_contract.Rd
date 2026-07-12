% Generated manually for the Rodeo vNext transformation contract.
\name{rodeo_transformation_contract}
\alias{rodeo_transformation_contract}
\alias{rodeo_transformation_spec}
\alias{rodeo_fit_transformation}
\alias{rodeo_apply_transformation}
\alias{rodeo_fit_apply_transformation}
\alias{rodeo_validate_transformation_schema}
\alias{rodeo_save_transformation}
\alias{rodeo_load_transformation}
\alias{rodeo_transformation_metadata}
\alias{qa_rodeo_transformation_contract}
\title{Structured Rodeo transformation contract}
\usage{
rodeo_transformation_spec(
  type = c("missing_impute", "constant_remove", "near_zero_variance_remove",
    "factor_levels", "date_features"),
  id = NULL,
  input_columns = character(),
  output_columns = NULL,
  parameters = list(),
  version = "0.1.0",
  metadata = list()
)

rodeo_fit_transformation(data, spec)

rodeo_apply_transformation(data, fitted_spec, copy_data = TRUE)

rodeo_fit_apply_transformation(data, spec, copy_data = TRUE)

rodeo_validate_transformation_schema(data, fitted_spec)

rodeo_save_transformation(fitted_spec, path)

rodeo_load_transformation(path)

rodeo_transformation_metadata(fitted_spec, data = NULL)

qa_rodeo_transformation_contract()
}
\arguments{
\item{type}{Transformation type.}
\item{id}{Optional unique transformation identifier.}
\item{input_columns}{Input columns required by the transformation. Empty means all columns at fit time.}
\item{output_columns}{Optional output columns for generated-column transformations.}
\item{parameters}{Named list of deterministic transformation parameters.}
\item{version}{Specification version.}
\item{metadata}{Optional user or system metadata.}
\item{data}{Training or scoring data.}
\item{spec}{A specification returned by \code{rodeo_transformation_spec()}.}
\item{fitted_spec}{A fitted specification returned by \code{rodeo_fit_transformation()}.}
\item{copy_data}{If \code{TRUE}, copy input data before applying the transformation.}
\item{path}{RDS path for saving or loading a fitted transformation.}
}
\description{
Creates, fits, applies, validates, serializes, loads, and inspects deterministic
Rodeo transformation specifications. The contract stores parameters and learned
state, not executable code.
}
\details{
The initial contract supports missing-value imputation, constant-column removal,
near-zero variance removal, factor-level management, and date feature extraction.
Fit consumes training data and stores learned state. Apply consumes a fitted
specification and must not recompute learned state.
}
\value{
Specification, fitted specification, transformed data, validation result,
metadata list, or QA result table depending on the function.
}
\examples{
data <- data.table::data.table(
  spend = c(1, NA, 3),
  channel = c("Search", NA, "Social")
)

spec <- rodeo_transformation_spec(
  type = "missing_impute",
  input_columns = c("spend", "channel"),
  parameters = list(method = "median_mode")
)

fitted <- rodeo_fit_transformation(data, spec)
prepared <- rodeo_apply_transformation(data, fitted)
metadata <- rodeo_transformation_metadata(fitted, prepared)
}
