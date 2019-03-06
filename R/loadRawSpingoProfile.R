require(readr)

#'
#'
#' @param spingoProfileLocation
#' @param taxonomyRank
loadRawSpingoProfile <- function(spingoProfileLocation = NULL, taxonomyRank = "species") {
  spingoProfile <- readr::read_tsv(spingoProfileLocation, col_names = c("spingoTaxonomyName", "spingoRelativeAbundance"))
  spingoProfile$rank <- taxonomyRank
  # colnames(spingoProfile) <- c("speciesName", "relativeAbundance")
  return(spingoProfile)
}
