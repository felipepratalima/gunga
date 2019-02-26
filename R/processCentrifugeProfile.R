require(magrittr)
require(taxdumpr)

#'
#'
#' @param rawCentrifugeProfile
processCentrifugeProfile <- function(rawCentrifugeProfile = NULL, taxdumprObject = NULL) {
  if (rawCentrifugeProfile %>% is.null) {
    stop("The rawCentrifugeProfile parameter should not be null")
  }

  if (rawCentrifugeProfile %>% is.na) {
    stop("The rawCentrifugeProfile parameter should not be NA")
  }

  if (rawCentrifugeProfile %>% nrow == 0) {
    stop("The rawCentrifugeProfile parameter should not be empty")
  }

  ## Get number of reads
  numberOfReads <- sum(rawCentrifugeProfile$numReads)
  numberOfUniqueReads <- sum(rawCentrifugeProfile$numReads)

  # Update Ids
  rawCentrifugeProfile$taxonomyId <- rawCentrifugeProfile$taxID
  rawCentrifugeProfile$taxonomyId <- rawCentrifugeProfile$taxonomyId %>% getUpdatedIds(taxdumprObject, .)

  # Standize Ids
  standardDf <- getStandardIdsDataFrame(unique(rawCentrifugeProfile$taxonomyId), taxdumprObject)
  rawCentrifugeProfile <- rawCentrifugeProfile %>% merge(standardDf, by = "taxonomyId")

  # Get complete taxonomy
  taxonomyDf <- unique(rawCentrifugeProfile$taxonomyId) %>% getStandardLineageIdsAndScientificNamesByIdsAsDataFrame(taxdumprObject, .)
  rawCentrifugeProfile <- rawCentrifugeProfile %>% merge(taxonomyDf, by = "taxonomyId")

  ## Aggregate taxonomic sublevels
  rawCentrifugeShProfileSummaries <- rawCentrifugeProfile %>%
    group_by(rank, standardId) %>%
    summarise(numberOfGenomes = n(),
              genomeSizeMean = mean(genomeSize),
              numReadsSum = sum(numReads),
              numReadsPercentual = sum(numReads) / numberOfReads,
              numUniqueReadsSum = sum(numUniqueReads),
              numUniqueReadsPercentual = sum(numUniqueReads) / numberOfReads,
              abundanceSum = sum(abundance))

  return(rawCentrifugeProfile)
}
