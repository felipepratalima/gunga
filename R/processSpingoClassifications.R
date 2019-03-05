require(magrittr)
require(taxdumpr)

#'
#'
#' @param rawSpingoClassifications
#' @param taxdumprObject
processSpingoClassifications <- function(rawSpingoClassifications = NULL, taxdumprObject = NULL) {
  if (rawSpingoClassifications %>% is.null) {
    stop("The rawSpingoClassifications parameter should not be null")
  }

  if (rawSpingoClassifications %>% is.na) {
    stop("The rawSpingoClassifications parameter should not be NA")
  }

  if (rawSpingoClassifications %>% nrow == 0) {
    stop("The rawSpingoClassifications parameter should not be empty")
  }

  # Process taxa names
  rawSpingoClassifications$spingoTaxonomyName <- rawSpingoClassifications$spingoSpeciesName %>% str_replace_all("_", " ")
  rawSpingoClassifications$taxonomyId <- rawSpingoClassifications$spingoTaxonomyName %>% getTaxonomyIdsByNames(taxdumprObject, .)

  rawSpingoClassifications$spingoTaxonomyName[rawSpingoClassifications$taxonomyId %>% is.na] <-
    rawSpingoClassifications$spingoGenusName[rawSpingoClassifications$taxonomyId %>% is.na]

  rawSpingoClassifications$taxonomyId[rawSpingoClassifications$taxonomyId %>% is.na] <-
    rawSpingoClassifications$spingoTaxonomyName[rawSpingoClassifications$taxonomyId %>% is.na] %>% getTaxonomyIdsByNames(taxdumprObject, .)

  # Standize Ids
  standardDf <- getStandardIdsDataFrame(unique(rawSpingoClassifications$taxonomyId), taxdumprObject)
  rawSpingoClassifications <- rawSpingoClassifications %>% merge(standardDf, by = "taxonomyId")

  # Get complete taxonomy
  taxonomyDf <- unique(rawSpingoClassifications$taxonomyId) %>% getStandardLineageIdsAndScientificNamesByIdsAsDataFrame(taxdumprObject, .)
  rawSpingoClassifications <- rawSpingoClassifications %>% merge(taxonomyDf, by = "taxonomyId")

  # Unrecognized speciesName
  speciesIndexes <-
    (rawSpingoClassifications$speciesId %>% is.na) &
    (rawSpingoClassifications$spingoSpeciesName %in% c("AMBIGUOUS", "UNCLASSIFIED")) == F
  rawSpingoClassifications$taxonomyId[speciesIndexes] <- rawSpingoClassifications$spingoSpeciesName[speciesIndexes]
  rawSpingoClassifications$taxonomyName[speciesIndexes] <- rawSpingoClassifications$spingoSpeciesName[speciesIndexes]
  rawSpingoClassifications$rank[speciesIndexes] <- "species"
  rawSpingoClassifications$standardId[speciesIndexes] <- rawSpingoClassifications$spingoSpeciesName[speciesIndexes]
  rawSpingoClassifications$speciesId[speciesIndexes] <- rawSpingoClassifications$spingoSpeciesName[speciesIndexes]
  rawSpingoClassifications$speciesName[speciesIndexes] <- rawSpingoClassifications$spingoSpeciesName[speciesIndexes]
  rawSpingoClassifications$spingoTaxonomyName[speciesIndexes] <- rawSpingoClassifications$spingoSpeciesName[speciesIndexes]

  # Unrecognized genusName
  genusIndexes <-
    (rawSpingoClassifications$genusId %>% is.na) &
    (rawSpingoClassifications$spingoGenusName %in% c("AMBIGUOUS", "UNCLASSIFIED")) == F
  rawSpingoClassifications$genusId[genusIndexes] <- rawSpingoClassifications$spingoGenusName[genusIndexes]
  rawSpingoClassifications$genusName[genusIndexes] <- rawSpingoClassifications$spingoGenusName[genusIndexes]

  # Unrecognized genusName and speciesName
  nonSpeciesIndexes <- (rawSpingoClassifications$speciesId %>% is.na)
  rawSpingoClassifications$standardId[genusIndexes & nonSpeciesIndexes] <- rawSpingoClassifications$spingoGenusName[genusIndexes & nonSpeciesIndexes]
  rawSpingoClassifications$taxonomyId[genusIndexes & nonSpeciesIndexes] <- rawSpingoClassifications$spingoGenusName[genusIndexes & nonSpeciesIndexes]
  rawSpingoClassifications$taxonomyName[genusIndexes & nonSpeciesIndexes] <- rawSpingoClassifications$spingoGenusName[genusIndexes & nonSpeciesIndexes]
  rawSpingoClassifications$rank[genusIndexes & nonSpeciesIndexes] <- "genus"
  rawSpingoClassifications$standardId[genusIndexes & nonSpeciesIndexes] <- rawSpingoClassifications$spingoGenusName[genusIndexes & nonSpeciesIndexes]
  rawSpingoClassifications$spingoTaxonomyName[genusIndexes & nonSpeciesIndexes] <- rawSpingoClassifications$spingoGenusName[genusIndexes & nonSpeciesIndexes]


  # # Standize Ids
  # standardDf <- getStandardIdsDataFrame(unique(rawSpingoClassifications$taxonomyId), taxdumprObject)
  # rawSpingoClassifications <- rawSpingoClassifications %>% merge(standardDf, by = "taxonomyId")
  #
  # # Get complete taxonomy
  # taxonomyDf <- unique(rawSpingoClassifications$taxonomyId) %>% getStandardLineageIdsAndScientificNamesByIdsAsDataFrame(taxdumprObject, .)
  # rawSpingoClassifications <- rawSpingoClassifications %>% merge(taxonomyDf, by = "taxonomyId")

  return(rawSpingoClassifications)
}
