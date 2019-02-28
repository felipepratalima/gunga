#'
#'
#' @param spingoClassificationsLocation
loadRawSpingoClassifications <- function(spingoClassificationsLocation = NULL) {
  # spingoClassifications <- read.delim(spingoClassificationsLocation, header = F, sep = "\t")
  # colnames(spingoClassifications) <- c("readId", "score", "clostridiumGroup", "clostridiumGroupScore", "genusName", "genusScore", "speciesName", "speciesScore")
  spingoClassifications <- readr::read_tsv(spingoClassificationsLocation,
                                           col_names = c("readId", "score", "clostridiumGroup", "clostridiumGroupScore", "genusName", "genusScore", "speciesName", "speciesScore"))
  return(spingoClassifications)
}
