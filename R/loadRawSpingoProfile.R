require(readr)

#'
#'
#' @param spingoProfileLocation
loadRawSpingoProfile <- function(spingoProfileLocation = NULL) {
  spingoProfile <- readr::read_tsv(spingoProfileLocation, col_names = c("spingoTaxonomyName", "spingoRelativeAbundance"))
  # spingoProfile$rank <- taxonomyRank
  # colnames(spingoProfile) <- c("speciesName", "relativeAbundance")
  return(spingoProfile)
}
