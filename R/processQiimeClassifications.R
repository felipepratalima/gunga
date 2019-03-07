processQiimeClassifications <- function(qiimeClassifications = NULL, taxdumprObject = NULL) {
  qiimeClassifications$qiimeLineage <- qiimeClassifications$qiimeLineage %>% as.character

  qiimeClassifications$qiimeSuperkingdomName <- str_extract(qiimeClassifications$qiimeLineage, "k__.*?(;|$)")
  qiimeClassifications$qiimeSuperkingdomName <- str_replace(qiimeClassifications$qiimeSuperkingdomName, "k__", "")
  qiimeClassifications$qiimeSuperkingdomName <- str_replace(qiimeClassifications$qiimeSuperkingdomName, "\\[", "")
  qiimeClassifications$qiimeSuperkingdomName <- str_replace(qiimeClassifications$qiimeSuperkingdomName, "\\]", "")
  qiimeClassifications$qiimeSuperkingdomName <- str_replace(qiimeClassifications$qiimeSuperkingdomName, ";", "")
  qiimeClassifications$qiimeSuperkingdomName <- ifelse(
    qiimeClassifications$qiimeSuperkingdomName != "",
    qiimeClassifications$qiimeSuperkingdomName,
    NA)

  qiimeClassifications$qiimePhylumName <- str_extract(qiimeClassifications$qiimeLineage, "p__.*?(;|$)")
  qiimeClassifications$qiimePhylumName <- str_replace(qiimeClassifications$qiimePhylumName, "p__", "")
  qiimeClassifications$qiimePhylumName <- str_replace(qiimeClassifications$qiimePhylumName, "\\[", "")
  qiimeClassifications$qiimePhylumName <- str_replace(qiimeClassifications$qiimePhylumName, "\\]", "")
  qiimeClassifications$qiimePhylumName <- str_replace(qiimeClassifications$qiimePhylumName, ";", "")
  qiimeClassifications$qiimePhylumName <- ifelse(
    qiimeClassifications$qiimePhylumName != "",
    qiimeClassifications$qiimePhylumName,
    NA)

  qiimeClassifications$qiimeClassName <- str_extract(qiimeClassifications$qiimeLineage, "c__.*?(;|$)")
  qiimeClassifications$qiimeClassName <- str_replace(qiimeClassifications$qiimeClassName, "c__", "")
  qiimeClassifications$qiimeClassName <- str_replace(qiimeClassifications$qiimeClassName, "\\[", "")
  qiimeClassifications$qiimeClassName <- str_replace(qiimeClassifications$qiimeClassName, "\\]", "")
  qiimeClassifications$qiimeClassName <- str_replace(qiimeClassifications$qiimeClassName, ";", "")
  qiimeClassifications$qiimeClassName <- ifelse(
    qiimeClassifications$qiimeClassName != "",
    qiimeClassifications$qiimeClassName,
    NA)

  qiimeClassifications$qiimeOrderName <- str_extract(qiimeClassifications$qiimeLineage, "o__.*?(;|$)")
  qiimeClassifications$qiimeOrderName <- str_replace(qiimeClassifications$qiimeOrderName, "o__", "")
  qiimeClassifications$qiimeOrderName <- str_replace(qiimeClassifications$qiimeOrderName, "\\[", "")
  qiimeClassifications$qiimeOrderName <- str_replace(qiimeClassifications$qiimeOrderName, "\\]", "")
  qiimeClassifications$qiimeOrderName <- str_replace(qiimeClassifications$qiimeOrderName, ";", "")
  qiimeClassifications$qiimeOrderName <- ifelse(
    qiimeClassifications$qiimeOrderName != "",
    qiimeClassifications$qiimeOrderName,
    NA)

  qiimeClassifications$qiimeFamilyName <- str_extract(qiimeClassifications$qiimeLineage, "f__.*?(;|$)")
  qiimeClassifications$qiimeFamilyName <- str_replace(qiimeClassifications$qiimeFamilyName, "f__", "")
  qiimeClassifications$qiimeFamilyName <- str_replace(qiimeClassifications$qiimeFamilyName, "\\[", "")
  qiimeClassifications$qiimeFamilyName <- str_replace(qiimeClassifications$qiimeFamilyName, "\\]", "")
  qiimeClassifications$qiimeFamilyName <- str_replace(qiimeClassifications$qiimeFamilyName, ";", "")
  qiimeClassifications$qiimeFamilyName <- ifelse(
    qiimeClassifications$qiimeFamilyName != "",
    qiimeClassifications$qiimeFamilyName,
    NA)

  qiimeClassifications$qiimeGenusName <- str_extract(qiimeClassifications$qiimeLineage, "g__.*?(;|$)")
  qiimeClassifications$qiimeGenusName <- str_replace(qiimeClassifications$qiimeGenusName, "g__", "")
  qiimeClassifications$qiimeGenusName <- str_replace(qiimeClassifications$qiimeGenusName, "\\[", "")
  qiimeClassifications$qiimeGenusName <- str_replace(qiimeClassifications$qiimeGenusName, "\\]", "")
  qiimeClassifications$qiimeGenusName <- str_replace(qiimeClassifications$qiimeGenusName, ";", "")
  qiimeClassifications$qiimeGenusName <- ifelse(
    qiimeClassifications$qiimeGenusName != "",
    qiimeClassifications$qiimeGenusName,
    NA)

  qiimeClassifications$qiimeSpeciesName <- str_extract(qiimeClassifications$qiimeLineage, "s__.*")
  qiimeClassifications$qiimeSpeciesName <- str_replace(qiimeClassifications$qiimeSpeciesName, "s__", "")
  qiimeClassifications$qiimeSpeciesName <- str_replace(qiimeClassifications$qiimeSpeciesName, "\\[", "")
  qiimeClassifications$qiimeSpeciesName <- str_replace(qiimeClassifications$qiimeSpeciesName, "\\]", "")
  qiimeClassifications$qiimeSpeciesName <- ifelse(
    qiimeClassifications$qiimeSpeciesName != "",
    paste(qiimeClassifications$qiimeGenusName, qiimeClassifications$qiimeSpeciesName),
    NA)

  qiimeClassifications$taxonomyId <-
    qiimeClassifications$qiimeSpeciesName %>% getTaxonomyIdsByNames(taxdumprObject, .)

  qiimeClassifications$taxonomyId[qiimeClassifications$taxonomyId %>% is.na] <-
    qiimeClassifications$qiimeGenusName[qiimeClassifications$taxonomyId %>% is.na] %>% getTaxonomyIdsByNames(taxdumprObject, .)

  qiimeClassifications$taxonomyId[qiimeClassifications$taxonomyId %>% is.na] <-
    qiimeClassifications$qiimeFamilyName[qiimeClassifications$taxonomyId %>% is.na] %>% getTaxonomyIdsByNames(taxdumprObject, .)

  qiimeClassifications$taxonomyId[qiimeClassifications$taxonomyId %>% is.na] <-
    qiimeClassifications$qiimeOrderName[qiimeClassifications$taxonomyId %>% is.na] %>% getTaxonomyIdsByNames(taxdumprObject, .)

  qiimeClassifications$taxonomyId[qiimeClassifications$taxonomyId %>% is.na] <-
    qiimeClassifications$qiimeClassName[qiimeClassifications$taxonomyId %>% is.na] %>% getTaxonomyIdsByNames(taxdumprObject, .)

  qiimeClassifications$taxonomyId[qiimeClassifications$taxonomyId %>% is.na] <-
    qiimeClassifications$qiimePhylumName[qiimeClassifications$taxonomyId %>% is.na] %>% getTaxonomyIdsByNames(taxdumprObject, .)

  qiimeClassifications$taxonomyId[qiimeClassifications$taxonomyId %>% is.na] <-
    qiimeClassifications$qiimeSuperkingdomName[qiimeClassifications$taxonomyId %>% is.na] %>% getTaxonomyIdsByNames(taxdumprObject, .)

  # Standize Ids
  standardDf <- getStandardIdsDataFrame(unique(qiimeClassifications$taxonomyId), taxdumprObject)
  qiimeClassifications <- qiimeClassifications %>% merge(standardDf, by = "taxonomyId")

  # Get complete taxonomy
  taxonomyDf <- unique(qiimeClassifications$taxonomyId) %>% getStandardLineageIdsAndScientificNamesByIdsAsDataFrame(taxdumprObject, .)
  qiimeClassifications <- qiimeClassifications %>% merge(taxonomyDf, by = "taxonomyId")

  return(qiimeClassifications)
}
