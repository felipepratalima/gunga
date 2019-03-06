require(taxdumpr)
require(magrittr)
require(stringr)
require(dplyr)

#'
#'
#' @param spingoProfile
#' @param spingoClassificationsSummaries
assignSpingoProfileAsAmplicons <- function(spingoProfile = NULL, spingoClassificationsSummaries = NULL) {
  if (spingoProfile %>% is.null) {
    stop("The spingoProfile parameter should not be null")
  }

  if (spingoProfile %>% is.na) {
    stop("The spingoProfile parameter should not be NA")
  }

  if (spingoClassificationsSummaries %>% is.null) {
    stop("The spingoClassificationsSummaries parameter should not be null")
  }

  if (spingoClassificationsSummaries %>% is.na) {
    stop("The spingoClassificationsSummaries parameter should not be NA")
  }

  ## Merging
  gungaProfile <- spingoProfile %>% merge(spingoClassificationsSummaries, by = c("rank", "standardId"))

  ## Update names
  namesIndexes <- (names(gungaProfile) %in% c("rank", "standardId") == FALSE)
  names(gungaProfile)[namesIndexes] <- names(gungaProfile)[namesIndexes] %>% paste0("ForAmp")

  ## Assign meta-omics origin
  gungaProfile$spingoAmp <- TRUE

  return(gungaProfile)
}
