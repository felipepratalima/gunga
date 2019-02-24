require(magrittr)
require(taxdumpr)

#'
#'
#' @param rawCentrifugeClassifications
processCentrifugeClassifications <- function(rawCentrifugeClassifications = NULL, taxdumprObject = NULL) {
  if (rawCentrifugeClassifications %>% is.null) {
    stop("The rawCentrifugeClassifications parameter should not be null")
  }

  if (rawCentrifugeClassifications %>% is.na) {
    stop("The rawCentrifugeClassifications parameter should not be NA")
  }

  if (rawCentrifugeClassifications %>% nrow == 0) {
    stop("The rawCentrifugeClassifications parameter should not be empty")
  }

  # Update Ids
  rawCentrifugeClassifications$taxonomyId <- rawCentrifugeClassifications$taxID
  rawCentrifugeClassifications$taxonomyId <- rawCentrifugeClassifications$taxonomyId %>% getUpdatedIds(taxdumprObject, .)

  # Standize Ids
  standardDf <- getStandardIdsDataFrame(unique(rawCentrifugeClassifications$taxonomyId), taxdumprObject)
  rawCentrifugeClassifications <- rawCentrifugeClassifications %>% merge(standardDf, by = "taxonomyId")

  # Get complete taxonomy
  taxonomyDf <- unique(rawCentrifugeClassifications$taxonomyId) %>% getStandardLineageIdsAndScientificNamesByIdsAsDataFrame(taxdumprObject, .)
  rawCentrifugeClassifications <- rawCentrifugeClassifications %>% merge(taxonomyDf, by = "taxonomyId")

  return(rawCentrifugeClassifications)
}
