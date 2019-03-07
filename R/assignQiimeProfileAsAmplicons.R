require(taxdumpr)
require(magrittr)
require(stringr)
require(dplyr)

#'
#'
#' @param qiimeProfile
#' @param qiimeClassificationsSummaries
assignQiimeProfileAsAmplicons <- function(qiimeProfile = NULL, qiimeClassificationsSummaries = NULL) {
  if (qiimeProfile %>% is.null) {
    stop("The qiimeProfile parameter should not be null")
  }

  if (qiimeProfile %>% is.na) {
    stop("The qiimeProfile parameter should not be NA")
  }

  if (qiimeClassificationsSummaries %>% is.null) {
    stop("The qiimeClassificationsSummaries parameter should not be null")
  }

  if (qiimeClassificationsSummaries %>% is.na) {
    stop("The qiimeClassificationsSummaries parameter should not be NA")
  }

  ## Merging
  gungaProfile <- qiimeProfile %>% merge(qiimeClassificationsSummaries, by = c("rank", "standardId"))

  ## Update names
  namesIndexes <- (names(gungaProfile) %in% c("rank", "standardId") == FALSE)
  names(gungaProfile)[namesIndexes] <- names(gungaProfile)[namesIndexes] %>% paste0("ForAmp")

  ## Assign meta-omics origin
  gungaProfile$qiimeAmp <- TRUE

  return(gungaProfile)
}
