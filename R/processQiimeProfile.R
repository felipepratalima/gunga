require(dplyr)
require(magrittr)
require(stringr)
require(taxdumpr)

#'
#'
#' @param qiimeProfile
.getDirectQiimeProfileSummaries <- function(qiimeProfile = NULL) {
  directProfileSummaries <- qiimeProfile %>%
    subset(standardId %>% is.na == F) %>%
    group_by(rank, standardId) %>%
    summarize(directQiimePercentageOfReads = sum(qiimePercentageOfReads))

  ## convert to char, beacuse sometimes we have non recognized taxonomies
  directProfileSummaries$standardId <- directProfileSummaries$standardId %>% as.character

  return(directProfileSummaries)
}


#'
#'
#' @param qiimeProfile
#' @param taxonomyRank
.getMappedQiimeProfileSummariesByRank <- function(qiimeProfile = NULL, taxonomyRank = "species") {
  summaryFieldName <- taxonomyRank %>% paste0("Id")
  summaryIndexes <- qiimeProfile[[summaryFieldName]] %>% is.na == F
  classificationsSummaries <-
    qiimeProfile %>%
    subset(summaryIndexes) %>%
    group_by_(summaryFieldName) %>%
    summarize(mappedQiimePercentageOfReads = sum(qiimePercentageOfReads)) %>%
    rename(standardId = summaryFieldName) %>%
    mutate(rank = taxonomyRank)

  return(classificationsSummaries)
}


#'
#'
#' @param qiimeProfile
#' @param taxonomyRank
processQiimeProfile <- function(qiimeProfile = NULL, taxdumprObject = NULL) {
  qiimeProfile$qiimeLineage <- qiimeProfile$qiimeLineage %>% as.character

  qiimeProfile$qiimeSuperkingdomName <- str_extract(qiimeProfile$qiimeLineage, "k__.*?(;|$)")
  qiimeProfile$qiimeSuperkingdomName <- str_replace(qiimeProfile$qiimeSuperkingdomName, "k__", "")
  qiimeProfile$qiimeSuperkingdomName <- str_replace(qiimeProfile$qiimeSuperkingdomName, "\\[", "")
  qiimeProfile$qiimeSuperkingdomName <- str_replace(qiimeProfile$qiimeSuperkingdomName, "\\]", "")
  qiimeProfile$qiimeSuperkingdomName <- str_replace(qiimeProfile$qiimeSuperkingdomName, ";", "")
  qiimeProfile$qiimeSuperkingdomName <- ifelse(
    qiimeProfile$qiimeSuperkingdomName != "",
    qiimeProfile$qiimeSuperkingdomName,
    NA)

  qiimeProfile$qiimePhylumName <- str_extract(qiimeProfile$qiimeLineage, "p__.*?(;|$)")
  qiimeProfile$qiimePhylumName <- str_replace(qiimeProfile$qiimePhylumName, "p__", "")
  qiimeProfile$qiimePhylumName <- str_replace(qiimeProfile$qiimePhylumName, "\\[", "")
  qiimeProfile$qiimePhylumName <- str_replace(qiimeProfile$qiimePhylumName, "\\]", "")
  qiimeProfile$qiimePhylumName <- str_replace(qiimeProfile$qiimePhylumName, ";", "")
  qiimeProfile$qiimePhylumName <- ifelse(
    qiimeProfile$qiimePhylumName != "",
    qiimeProfile$qiimePhylumName,
    NA)

  qiimeProfile$qiimeClassName <- str_extract(qiimeProfile$qiimeLineage, "c__.*?(;|$)")
  qiimeProfile$qiimeClassName <- str_replace(qiimeProfile$qiimeClassName, "c__", "")
  qiimeProfile$qiimeClassName <- str_replace(qiimeProfile$qiimeClassName, "\\[", "")
  qiimeProfile$qiimeClassName <- str_replace(qiimeProfile$qiimeClassName, "\\]", "")
  qiimeProfile$qiimeClassName <- str_replace(qiimeProfile$qiimeClassName, ";", "")
  qiimeProfile$qiimeClassName <- ifelse(
    qiimeProfile$qiimeClassName != "",
    qiimeProfile$qiimeClassName,
    NA)

  qiimeProfile$qiimeOrderName <- str_extract(qiimeProfile$qiimeLineage, "o__.*?(;|$)")
  qiimeProfile$qiimeOrderName <- str_replace(qiimeProfile$qiimeOrderName, "o__", "")
  qiimeProfile$qiimeOrderName <- str_replace(qiimeProfile$qiimeOrderName, "\\[", "")
  qiimeProfile$qiimeOrderName <- str_replace(qiimeProfile$qiimeOrderName, "\\]", "")
  qiimeProfile$qiimeOrderName <- str_replace(qiimeProfile$qiimeOrderName, ";", "")
  qiimeProfile$qiimeOrderName <- ifelse(
    qiimeProfile$qiimeOrderName != "",
    qiimeProfile$qiimeOrderName,
    NA)

  qiimeProfile$qiimeFamilyName <- str_extract(qiimeProfile$qiimeLineage, "f__.*?(;|$)")
  qiimeProfile$qiimeFamilyName <- str_replace(qiimeProfile$qiimeFamilyName, "f__", "")
  qiimeProfile$qiimeFamilyName <- str_replace(qiimeProfile$qiimeFamilyName, "\\[", "")
  qiimeProfile$qiimeFamilyName <- str_replace(qiimeProfile$qiimeFamilyName, "\\]", "")
  qiimeProfile$qiimeFamilyName <- str_replace(qiimeProfile$qiimeFamilyName, ";", "")
  qiimeProfile$qiimeFamilyName <- ifelse(
    qiimeProfile$qiimeFamilyName != "",
    qiimeProfile$qiimeFamilyName,
    NA)

  qiimeProfile$qiimeGenusName <- str_extract(qiimeProfile$qiimeLineage, "g__.*?(;|$)")
  qiimeProfile$qiimeGenusName <- str_replace(qiimeProfile$qiimeGenusName, "g__", "")
  qiimeProfile$qiimeGenusName <- str_replace(qiimeProfile$qiimeGenusName, "\\[", "")
  qiimeProfile$qiimeGenusName <- str_replace(qiimeProfile$qiimeGenusName, "\\]", "")
  qiimeProfile$qiimeGenusName <- str_replace(qiimeProfile$qiimeGenusName, ";", "")
  qiimeProfile$qiimeGenusName <- ifelse(
    qiimeProfile$qiimeGenusName != "",
    qiimeProfile$qiimeGenusName,
    NA)

  qiimeProfile$qiimeSpeciesName <- str_extract(qiimeProfile$qiimeLineage, "s__.*")
  qiimeProfile$qiimeSpeciesName <- str_replace(qiimeProfile$qiimeSpeciesName, "s__", "")
  qiimeProfile$qiimeSpeciesName <- str_replace(qiimeProfile$qiimeSpeciesName, "\\[", "")
  qiimeProfile$qiimeSpeciesName <- str_replace(qiimeProfile$qiimeSpeciesName, "\\]", "")
  qiimeProfile$qiimeSpeciesName <- ifelse(
    qiimeProfile$qiimeSpeciesName != "",
    paste(qiimeProfile$qiimeGenusName, qiimeProfile$qiimeSpeciesName),
    NA)

  qiimeProfile$taxonomyId <-
    qiimeProfile$qiimeSpeciesName %>% getTaxonomyIdsByNames(taxdumprObject, .)

  qiimeProfile$taxonomyId[qiimeProfile$taxonomyId %>% is.na] <-
    qiimeProfile$qiimeGenusName[qiimeProfile$taxonomyId %>% is.na] %>% getTaxonomyIdsByNames(taxdumprObject, .)

  qiimeProfile$taxonomyId[qiimeProfile$taxonomyId %>% is.na] <-
    qiimeProfile$qiimeFamilyName[qiimeProfile$taxonomyId %>% is.na] %>% getTaxonomyIdsByNames(taxdumprObject, .)

  qiimeProfile$taxonomyId[qiimeProfile$taxonomyId %>% is.na] <-
    qiimeProfile$qiimeOrderName[qiimeProfile$taxonomyId %>% is.na] %>% getTaxonomyIdsByNames(taxdumprObject, .)

  qiimeProfile$taxonomyId[qiimeProfile$taxonomyId %>% is.na] <-
    qiimeProfile$qiimeClassName[qiimeProfile$taxonomyId %>% is.na] %>% getTaxonomyIdsByNames(taxdumprObject, .)

  qiimeProfile$taxonomyId[qiimeProfile$taxonomyId %>% is.na] <-
    qiimeProfile$qiimePhylumName[qiimeProfile$taxonomyId %>% is.na] %>% getTaxonomyIdsByNames(taxdumprObject, .)

  qiimeProfile$taxonomyId[qiimeProfile$taxonomyId %>% is.na] <-
    qiimeProfile$qiimeSuperkingdomName[qiimeProfile$taxonomyId %>% is.na] %>% getTaxonomyIdsByNames(taxdumprObject, .)

  # Standize Ids
  standardDf <- getStandardIdsDataFrame(unique(qiimeProfile$taxonomyId), taxdumprObject)
  qiimeProfile <- qiimeProfile %>% merge(standardDf, by = "taxonomyId")

  # Get complete taxonomy
  taxonomyDf <- unique(qiimeProfile$taxonomyId) %>% getStandardLineageIdsAndScientificNamesByIdsAsDataFrame(taxdumprObject, .)
  qiimeProfile <- qiimeProfile %>% merge(taxonomyDf, by = "taxonomyId")

  ## Summarize
  directProfileSummaries <- .getDirectQiimeProfileSummaries(qiimeProfile)

  superkingdomProfileSummaries <- .getMappedQiimeProfileSummariesByRank(qiimeProfile, "superkingdom")
  phylumProfileSummaries <- .getMappedQiimeProfileSummariesByRank(qiimeProfile, "phylum")
  classProfileSummaries <- .getMappedQiimeProfileSummariesByRank(qiimeProfile, "class")
  orderProfileSummaries <- .getMappedQiimeProfileSummariesByRank(qiimeProfile, "order")
  familyProfileSummaries <- .getMappedQiimeProfileSummariesByRank(qiimeProfile, "family")
  genusProfileSummaries <- .getMappedQiimeProfileSummariesByRank(qiimeProfile, "genus")
  speciesProfileSummaries <- .getMappedQiimeProfileSummariesByRank(qiimeProfile, "species")

  mappedProfileSummaries <- rbind(
    superkingdomProfileSummaries,
    phylumProfileSummaries,
    classProfileSummaries,
    orderProfileSummaries,
    familyProfileSummaries,
    genusProfileSummaries,
    speciesProfileSummaries
  )

  qiimeProfileSummaries <- directProfileSummaries %>%
    merge(mappedProfileSummaries, by = c("rank", "standardId"), all = T)

  return(qiimeProfileSummaries)
}
