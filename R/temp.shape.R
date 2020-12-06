#' Temporary shape file name
#' @return path
#'
temp.shp <- function() {
  temp.file <-
    file.path(tempdir(),
              paste0(paste0(sample(c(LETTERS, letters, 1:9), 8, replace = T),
                            collapse = ""), ".shp"))
  return(temp.file)
}

#' Remove temporary shape files
#' @param file file name
#' @param fileEnding default .shp
#'
unlink.shp <- function(file = NULL, fileEnding = ".shp") {
  ## make names
  to_unlink <- stringr::str_replace(string = file, pattern = ".shp",
                                    replacement = c(".shp", ".dbf", ".prj", ".shx", ".qpj", ".mshp"))
  silent <- sapply(to_unlink, unlink)
  return(data.frame(file = to_unlink,
                    success = ifelse(silent == 0, "Yes", "No")))
}
