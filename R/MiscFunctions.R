#' @title BuildBinary
#'
#' @description Build package binary
#'
#' @author Adrian Antico
#'
#' @family Utilities
#'
#' @param Root NULL will setwd to project root as defined in function
#'
#' @export
BuildBinary <- function(Root = NULL) {
  x <- getwd()
  if(!is.null(Root)) {
    setwd(Root)
    devtools::install(pkg = "Rodeo", dependencies = FALSE)
  } else {
    setwd("C:/Users/Bizon/Documents/GitHub")
    devtools::build(pkg = "Rodeo")
  }
  setwd(x)
}

#' @title Install
#'
#' @description To install the package
#'
#' @author Adrian Antico
#'
#' @family Utilities
#'
#' @param Root NULL will setwd to project root as defined in function
#'
#' @export
Install <- function(Root = NULL) {
  x <- getwd()
  if(!is.null(Root)) {
    setwd(Root)
    devtools::install(pkg = "Rodeo", dependencies = FALSE)
  } else {
    setwd("C:/Users/Bizon/Documents/GitHub")
    devtools::install(pkg = "Rodeo", dependencies = FALSE)
  }
  setwd(x)
}

#' @title UpdateDocs
#'
#' @description Update helf files and reference manual
#'
#' @author Adrian Antico
#'
#' @family Utilities
#'
#' @export
UpdateDocs <- function(BuildVignette = FALSE, Root = NULL) {
  x <- getwd()
  if(!is.null(Root)) {
    setwd(Root)
    devtools::document()
    if(BuildVignette) devtools::build_manual()
  } else {
    setwd("C:/Users/Bizon/Documents/GitHub/Rodeo")
    devtools::document()
    if(BuildVignette) devtools::build_manual()
  }
  setwd(x)
}

#' @title FullFactorialCatFeatures
#'
#' @description FullFactorialCatFeatures reverses the difference
#'
#' @family Data Wrangling
#'
#' @author Adrian Antico
#'
#' @param GroupVars Character vector of categorical columns to fully interact
#' @param MaxCombin The max K in N choose K. If NULL, K will loop through 1 to length(GroupVars)
#' @param BottomsUp TRUE or FALSE. TRUE starts with the most comlex interaction to the main effects
#'
#' @noRd
FullFactorialCatFeatures <- function(GroupVars = NULL,
                                     MaxCombin = NULL,
                                     BottomsUp = TRUE) {

  if(is.null(MaxCombin)) {
    MaxCombin <- N <- length(GroupVars)
  } else {
    N <- MaxCombin
  }
  Categoricals <- c()

  # N choose 1 case
  for(j in seq_along(GroupVars)) Categoricals <- c(Categoricals,GroupVars[j])

  # N choose i for 2 <= i < N
  for(i in seq_len(N)[-1L]) {

    # Case 2: N choose 2 up to N choose N-1: Middle-Hierarchy Interactions
    if(MaxCombin == length(GroupVars)) {
      if(i < N) {
        temp <- combinat::combn(GroupVars, m = i)
        temp2 <- c()
        for(k in seq_len(ncol(temp))) {
          for(l in seq_len(i)) {
            if(l == 1L) {
              temp2 <- temp[l,k]
            } else {
              temp2 <- paste(temp2,temp[l,k], sep = '_')
            }
          }
          Categoricals <- c(Categoricals, temp2)
        }

        # Case 3: N choose N - Full Interaction
      } else if(i == length(GroupVars)) {
        temp <- combinat::combn(GroupVars, m = i)
        for(m in seq_len(N)) {
          if(m == 1) {
            temp2 <- temp[m]
          } else {
            temp2 <- paste(temp2,temp[m], sep = '_')
          }
        }
        Categoricals <- c(Categoricals, temp2)
      }
    } else {
      if(i <= N) {
        temp <- combinat::combn(GroupVars, m = i)
        temp2 <- c()
        for(k in seq_len(ncol(temp))) {
          for(l in seq_len(i)) {
            if(l == 1L) {
              temp2 <- temp[l,k]
            } else {
              temp2 <- paste(temp2,temp[l,k], sep = '_')
            }
          }
          Categoricals <- c(Categoricals, temp2)
        }

        # Case 3: N choose N - Full Interaction
      } else if(i == length(GroupVars)) {
        temp <- combinat::combn(GroupVars, m = i)
        for(m in seq_len(N)) {
          if(m == 1) {
            temp2 <- temp[m]
          } else {
            temp2 <- paste(temp2,temp[m], sep = '_')
          }
        }
        Categoricals <- c(Categoricals, temp2)
      }
    }

  }

  # Order of output
  if(BottomsUp) return(rev(Categoricals)) else return(Categoricals)
}
