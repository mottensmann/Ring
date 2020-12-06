#' Open all lookup tables as a tab
#'
#' @param Lookup Name of lookups.mde driver
#' @details See \link{export.ring} and \link{check.ring}
#' @return list of data frame
#'
#' @import RODBC magrittr
#' @export
#'
read.lookup <- function(Lookup = NULL) {

  ## Connect ...
  ## ===========================================================================
  ch3 <- RODBC::odbcConnect('Lookup')
  ## ===========================================================================

  ## Query tables
  ## ===========================================================================
  lookup <- RODBC::sqlTables(ch3, tableType = "TABLE")[["TABLE_NAME"]] %>%
     lapply(., function(x) RODBC::sqlFetch(ch3, as.character(x))) %>%
     set_names(., RODBC::sqlTables(ch3, tableType = "TABLE")[["TABLE_NAME"]])
  ## ===========================================================================

}
