require(readr)

#'
#'
#' @param spingoProfileLocation
loadRawSpingoProfile <- function(spingoProfileLocation = NULL) {
  spingoProfile <- readr::read_tsv(spingoProfileLocation, col_names = c("speciesName", "relativeAbundance"))
  # colnames(spingoProfile) <- c("speciesName", "relativeAbundance")
  return(spingoProfile)
}
