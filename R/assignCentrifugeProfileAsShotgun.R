require(taxdumpr)
require(magrittr)
require(stringr)
require(dplyr)

#'
#'
#' @param centrifugeProfile
#' @param centrifugeClassificationsSummaries
assignCentrifugeProfileAsShotgun <- function(centrifugeProfile = NULL, centrifugeClassificationsSummaries = NULL) {
  if (centrifugeProfile %>% is.null) {
    stop("The centrifugeProfile parameter should not be null")
  }

  if (centrifugeProfile %>% is.na) {
    stop("The centrifugeProfile parameter should not be NA")
  }

  if (centrifugeClassificationsSummaries %>% is.null) {
    stop("The centrifugeClassificationsSummaries parameter should not be null")
  }

  if (centrifugeClassificationsSummaries %>% is.na) {
    stop("The centrifugeClassificationsSummaries parameter should not be NA")
  }

  ## Merging
  gungaProfile <- centrifugeProfile %>% merge(centrifugeClassificationsSummaries, by = c("rank", "standardId"))

  ## Update names
  namesIndexes <- (names(gungaProfile) %in% c("rank", "standardId") == FALSE)
  names(gungaProfile)[namesIndexes] <- names(gungaProfile)[namesIndexes] %>% paste0("ForSh")

  ## Assign meta-omics origin
  gungaProfile$centrifugeSh <- TRUE

  return(gungaProfile)
}
