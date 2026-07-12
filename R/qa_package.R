#' Run Rodeo installed-package QA
#'
#' @description
#' Runs the stable installed-package QA contract for Rodeo. This is the public
#' QA entry point intended for package refresh and cross-repository validation.
#' Individual `qa_*` helpers remain implementation-specific and should not be
#' treated as long-term public API.
#'
#' @return A data.table with QA check rows and normalized status values.
#'
#' @examples
#' \dontrun{
#' qa_rodeo_package()
#' }
#'
#' @export
qa_rodeo_package <- function() {
  rows <- data.table::rbindlist(list(
    qa_rodeo_vnext(),
    qa_rodeo_transformation_contract(),
    qa_rodeo_vnext_model_prep(),
    qa_generate_rodeo_model_prep_artifacts()
  ), use.names = TRUE, fill = TRUE)

  if ("passed" %in% names(rows) && !"status" %in% names(rows)) {
    rows[, status := ifelse(isTRUE(passed), "success", "error"), by = seq_len(nrow(rows))]
  }
  if (!"message" %in% names(rows)) {
    if ("detail" %in% names(rows)) {
      rows[, message := as.character(detail)]
    } else {
      rows[, message := ""]
    }
  }
  rows
}
