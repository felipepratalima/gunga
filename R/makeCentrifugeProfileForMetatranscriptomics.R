require(taxdumpr)
require(magrittr)
require(stringr)
require(dplyr)

#'
#'
#' @param taxdumprObject
makeCentrifugeProfileForMetatranscriptomics <- function(rawCentrifugeProfileDf = NULL, rawCentrifugeClassificationsDf = NULL, taxdumprObject = NULL) {
  if (rawCentrifugeProfileDf %>% is.null) {
    stop("The rawCentrifugeProfileDf parameter should not be null")
  }

  if (rawCentrifugeProfileDf %>% is.na) {
    stop("The rawCentrifugeProfileDf parameter should not be NA")
  }

  if (rawCentrifugeClassificationsDf %>% is.null) {
    stop("The rawCentrifugeClassificationsDf parameter should not be null")
  }

  if (rawCentrifugeClassificationsDf %>% is.na) {
    stop("The rawCentrifugeClassificationsDf parameter should not be NA")
  }

  if (taxdumprObject %>% is.null) {
    stop("The taxdumprObject parameter should not be null")
  }

  if (taxdumprObject %>% is.na) {
    stop("The taxdumprObject parameter should not be NA")
  }

  ##
  centrifugeProfileDf <- rawCentrifugeProfileDf
  centrifugeProfileDf$taxonomyId <- centrifugeProfileDf$taxID
  # centrifugeProfileDf <- centrifugeProfileDf %>% subset(taxonomyId > 1)

  ##
  taxonomyIds <- centrifugeProfileDf$taxonomyId %>% unique
  standardIds <- getStandardTaxonomyIdsByIds(x = taxdumprObject, taxonomyIds = taxonomyIds)
  centrifugeProfileDf <- centrifugeProfileDf %>% merge(data.frame(taxonomyId = taxonomyIds, standardId = standardIds))
  centrifugeProfileDf$rank <- getTaxonomyRanksByIds(x = taxdumprObject, taxonomyIds = centrifugeProfileDf$standardId)
  centrifugeProfileDf <- centrifugeProfileDf %>% group_by(standardId, rank) %>%
    summarise(abundance = sum(abundance), numUniqueReads = sum(numUniqueReads), genomeSize = mean(genomeSize))

  ##
  numberOfSequences <- nrow(rawCentrifugeClassificationsDf)
  centrifugeClassificationsDf <- rawCentrifugeClassificationsDf
  centrifugeClassificationsDf$taxonomyId <- centrifugeClassificationsDf$taxID
  centrifugeClassificationsDf <- centrifugeClassificationsDf %>% subset(taxonomyId > 1)

  ##
  taxonomyIds <- centrifugeClassificationsDf$taxonomyId %>% unique
  standardIds <- getStandardTaxonomyIdsByIds(x = taxdumprObject, taxonomyIds = taxonomyIds)
  centrifugeClassificationsDf <- centrifugeClassificationsDf %>% merge(data.frame(taxonomyId = taxonomyIds, standardId = standardIds))
  # centrifugeClassificationsDf$rank <- getTaxonomyRanksByIds(x = taxdumprObject, taxonomyIds = centrifugeClassificationsDf$standardId)

  ##
  # taxonomyDf <- getStandardLineageIdsAndScientificNamesByIdsAsDataFrame(x = taxdumprObject, taxonomyIds = unique(standardIds))
  # taxonomyDf$standardId <- taxonomyDf$taxonomyId

  ##
  centrifugeClassificationsSummariesDf <- centrifugeClassificationsDf %>%
    select(standardId, score, hitLength, queryLength, numMatches) %>%
    group_by(standardId) %>%
    summarise(scoreMedian = median(score),
              scoreCI = qnorm(0.975) * sd(score) / sqrt(length(score)),
              hitLengthMedian = median(hitLength),
              hitLengthCI = qnorm(0.975) * sd(hitLength) / sqrt(length(hitLength)),
              queryLengthMedian = median(queryLength),
              queryLengthCI = qnorm(0.975) * sd(queryLength) / sqrt(length(queryLength)),
              numberOfMatchesMedian = median(numMatches),
              numberOfMatchesCI = qnorm(0.975) * sd(numMatches) / sqrt(length(numMatches)))

  ##
  gungaProfile <- centrifugeProfileDf %>% subset(rank == "species") %>% merge(centrifugeClassificationsSummariesDf, by = "standardId")
  gungaProfile$rnaCentrifugePercentual <- gungaProfile$numUniqueReads / numberOfSequences
  gungaProfile <- gungaProfile %>% select(speciesId = standardId,
                                                    rnaCentrifugePercentual,
                                                    rnaCentrifugeScoreMedian = scoreMedian,
                                                    rnaCentrifugeScoreCI = scoreCI,
                                                    rnaCentrifugeHitLengthMedian = hitLengthMedian,
                                                    rnaCentrifugeHitLengthCI = hitLengthCI,
                                                    rnaCentrifugeQueryLengthMedian = queryLengthMedian,
                                                    rnaCentrifugeQueryLengthCI = queryLengthCI,
                                                    rnaCentrifugeNumberOfMatchesMedian = numberOfMatchesMedian,
                                                    rnaCentrifugeNumberOfMatchesCI = numberOfMatchesCI)


  ##
  gungaProfile$centrifugeMt <- TRUE
  return(gungaProfile)
}

# rawCentrifugeProfileDf <- loadRawCentrifugeProfile("data-raw/mt/centrifuge_report.tsv")
# rawCentrifugeClassificationsDf <- loadRawCentrifugeClassifications("data-raw/mt/centrifuge_classifications.tsv")
# taxdumprObject <- Taxdumpr("~/taxdump/nodes.dmp", "~/taxdump/names.dmp", "~/taxdump/merged.dmp")
# centrifugeGungaProfileForMt <- makeCentrifugeProfileForMetatranscriptomics(rawCentrifugeProfileDf, rawCentrifugeClassificationsDf, taxdumprObject)
# nrow(centrifugeGungaProfileForMt)
