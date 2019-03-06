require(magrittr)
require(taxdumpr)

#'
#'
#' @param rawSpingoSpeciesProfile
#' @param rawSpingoGenusProfile
#' @param taxdumprObject
processSpingoProfile <- function(rawSpingoSpeciesProfileDf = NULL, rawSpingoGenusProfileDf = NULL, taxdumprObject = NULL) {
  # if (rawSpingoSpeciesProfileDf %>% is.null) {
  #   stop("The rawSpingoSpeciesProfile parameter should not be null")
  # }
  #
  # if (rawSpingoSpeciesProfileDf %>% is.na) {
  #   stop("The rawSpingoSpeciesProfile parameter should not be NA")
  # }
  #
  # if (rawSpingoSpeciesProfileDf %>% nrow == 0) {
  #   stop("The rawSpingoSpeciesProfile parameter should not be empty")
  # }

  ## Species
  rawSpingoSpeciesProfileDf <- rawSpingoSpeciesProfileDf %>% subset(spingoTaxonomyName != "(AMBIGUOUS") %>% subset(spingoTaxonomyName != "UNCLASSIFIED")
  rawSpingoSpeciesProfileDf$spingoRelativeAbundance <- rawSpingoSpeciesProfileDf$spingoRelativeAbundance %>% as.numeric
  rawSpingoSpeciesProfileDf$spingoTaxonomyName <- rawSpingoSpeciesProfileDf$spingoTaxonomyName %>% str_replace_all("_", " ")
  rawSpingoSpeciesProfileDf$taxonomyId <- rawSpingoSpeciesProfileDf$spingoTaxonomyName %>% getStandardTaxonomyIdsByNames(taxdumprObject, .)
  rawSpingoSpeciesProfileDf$standardId <- rawSpingoSpeciesProfileDf$taxonomyId
  rawSpingoSpeciesProfileDf$rank <- "species"

  taxonomyDf <- unique(rawSpingoSpeciesProfileDf$taxonomyId) %>% getStandardLineageIdsAndScientificNamesByIdsAsDataFrame(taxdumprObject, .)
  rawSpingoSpeciesProfileDf <- rawSpingoSpeciesProfileDf %>% merge(taxonomyDf, by = "taxonomyId")

  rawSpingoSpeciesProfileDf$speciesId[rawSpingoSpeciesProfileDf$taxonomyId %>% is.na] <-
    rawSpingoSpeciesProfileDf$spingoTaxonomyName[rawSpingoSpeciesProfileDf$taxonomyId %>% is.na]

  rawSpingoSpeciesProfileDf$speciesName[rawSpingoSpeciesProfileDf$taxonomyId %>% is.na] <-
    rawSpingoSpeciesProfileDf$spingoTaxonomyName[rawSpingoSpeciesProfileDf$taxonomyId %>% is.na]

  rawSpingoSpeciesProfileDf$genusName[rawSpingoSpeciesProfileDf$genusId %>% is.na] <-
    rawSpingoSpeciesProfileDf$spingoTaxonomyName[rawSpingoSpeciesProfileDf$genusId %>% is.na] %>%
    str_split(" ") %>%
    unlist %>%
    .[1]

  rawSpingoSpeciesProfileDf$genusId[rawSpingoSpeciesProfileDf$genusId %>% is.na] <-
    rawSpingoSpeciesProfileDf$spingoTaxonomyName[rawSpingoSpeciesProfileDf$genusId %>% is.na] %>%
    str_split(" ") %>%
    unlist %>%
    .[1]

  rawSpingoSpeciesProfileDf$standardId[rawSpingoSpeciesProfileDf$taxonomyId %>% is.na] <-
    rawSpingoSpeciesProfileDf$spingoTaxonomyName[rawSpingoSpeciesProfileDf$taxonomyId %>% is.na]

  rawSpingoSpeciesProfileDf$taxonomyId[rawSpingoSpeciesProfileDf$taxonomyId %>% is.na] <-
    rawSpingoSpeciesProfileDf$spingoTaxonomyName[rawSpingoSpeciesProfileDf$taxonomyId %>% is.na]

  rawSpingoSpeciesProfileDf$spingoSpeciesRelativeAbundance <- rawSpingoSpeciesProfileDf$spingoRelativeAbundance
  rawSpingoSpeciesProfileDf$spingoSpeciesClassifiedRelativeAbundance <-
    rawSpingoSpeciesProfileDf$spingoSpeciesRelativeAbundance /
    sum(rawSpingoSpeciesProfileDf$spingoSpeciesRelativeAbundance, na.rm = TRUE)

  ## Genus
  rawSpingoGenusProfileDf <- rawSpingoGenusProfileDf %>% subset(spingoTaxonomyName != "(AMBIGUOUS") %>% subset(spingoTaxonomyName != "UNCLASSIFIED")
  rawSpingoGenusProfileDf$spingoRelativeAbundance <- rawSpingoGenusProfileDf$spingoRelativeAbundance %>% as.numeric
  rawSpingoGenusProfileDf$taxonomyId <- rawSpingoGenusProfileDf$spingoTaxonomyName %>% getStandardTaxonomyIdsByNames(taxdumprObject, .)
  rawSpingoGenusProfileDf$standardId <- rawSpingoGenusProfileDf$taxonomyId
  rawSpingoGenusProfileDf$rank <- "genus"

  taxonomyDf <- unique(rawSpingoGenusProfileDf$taxonomyId) %>% getStandardLineageIdsAndScientificNamesByIdsAsDataFrame(taxdumprObject, .)
  rawSpingoGenusProfileDf <- rawSpingoGenusProfileDf %>% merge(taxonomyDf, by = "taxonomyId")

  rawSpingoGenusProfileDf$genusId[rawSpingoGenusProfileDf$taxonomyId %>% is.na] <-
    rawSpingoGenusProfileDf$spingoTaxonomyName[rawSpingoGenusProfileDf$taxonomyId %>% is.na]

  rawSpingoGenusProfileDf$genusName[rawSpingoGenusProfileDf$taxonomyId %>% is.na] <-
    rawSpingoGenusProfileDf$spingoTaxonomyName[rawSpingoGenusProfileDf$taxonomyId %>% is.na]

  rawSpingoGenusProfileDf$standardId[rawSpingoGenusProfileDf$taxonomyId %>% is.na] <-
    rawSpingoGenusProfileDf$spingoTaxonomyName[rawSpingoGenusProfileDf$taxonomyId %>% is.na]

  rawSpingoGenusProfileDf$taxonomyId[rawSpingoGenusProfileDf$taxonomyId %>% is.na] <-
    rawSpingoGenusProfileDf$spingoTaxonomyName[rawSpingoGenusProfileDf$taxonomyId %>% is.na]

  rawSpingoGenusProfileDf$spingoGenusRelativeAbundance <- rawSpingoGenusProfileDf$spingoRelativeAbundance
  rawSpingoGenusProfileDf$spingoGenusClassifiedRelativeAbundance <-
    rawSpingoGenusProfileDf$spingoGenusRelativeAbundance /
    sum(rawSpingoGenusProfileDf$spingoGenusRelativeAbundance, na.rm = TRUE)

  ## Complete species and genus profiles
  genusInformation <- rawSpingoGenusProfileDf %>% select(genusId, spingoGenusRelativeAbundance, spingoGenusClassifiedRelativeAbundance)
  rawSpingoSpeciesProfileDf <- rawSpingoSpeciesProfileDf %>% merge(genusInformation, by = "genusId", all.x = T)
  rawSpingoSpeciesProfileDf$spingoGenusRelativeAbundance[rawSpingoSpeciesProfileDf$spingoGenusRelativeAbundance %>% is.na] <-
    rawSpingoSpeciesProfileDf$spingoSpeciesRelativeAbundance[rawSpingoSpeciesProfileDf$spingoGenusRelativeAbundance %>% is.na]
  rawSpingoSpeciesProfileDf$spingoGenusClassifiedRelativeAbundance[rawSpingoSpeciesProfileDf$spingoGenusClassifiedRelativeAbundance %>% is.na] <-
    rawSpingoSpeciesProfileDf$spingoSpeciesClassifiedRelativeAbundance[rawSpingoSpeciesProfileDf$spingoGenusClassifiedRelativeAbundance %>% is.na]

  speciesInformation <- rawSpingoSpeciesProfileDf %>% select(genusId, spingoSpeciesRelativeAbundance, spingoSpeciesClassifiedRelativeAbundance)
  rawSpingoGenusProfileDf <- rawSpingoGenusProfileDf %>% merge(speciesInformation, by = "genusId", all.x = T)

  ## Family
  spingoFamilyProfileFromGenusDf <- rawSpingoGenusProfileDf %>%
    group_by(familyId) %>%
    summarize(spingoGenusRelativeAbundance = sum(spingoRelativeAbundance)) %>%
    rename(standardId = familyId) %>%
    mutate(rank = "family")
  spingoFamilyProfileFromGenusDf$spingoGenusClassifiedRelativeAbundance <-
    spingoFamilyProfileFromGenusDf$spingoGenusRelativeAbundance /
    sum(spingoFamilyProfileFromGenusDf$spingoGenusRelativeAbundance, na.rm = TRUE)

  spingoFamilyProfileFromSpeciesDf <- rawSpingoSpeciesProfileDf %>%
    group_by(familyId) %>%
    summarize(spingoSpeciesRelativeAbundance = sum(spingoRelativeAbundance)) %>%
    rename(standardId = familyId) %>%
    mutate(rank = "family")
  spingoFamilyProfileFromSpeciesDf$spingoSpeciesClassifiedRelativeAbundance <-
    spingoFamilyProfileFromSpeciesDf$spingoSpeciesRelativeAbundance /
    sum(spingoFamilyProfileFromSpeciesDf$spingoSpeciesRelativeAbundance, na.rm = TRUE)

  spingoFamilyProfileDf <- spingoFamilyProfileFromGenusDf %>%
    merge(spingoFamilyProfileFromSpeciesDf, by = c("rank", "standardId"), all = T)

  ## Order
  spingoOrderProfileFromGenusDf <- rawSpingoGenusProfileDf %>%
    group_by(orderId) %>%
    summarize(spingoGenusRelativeAbundance = sum(spingoRelativeAbundance)) %>%
    rename(standardId = orderId) %>%
    mutate(rank = "order")
  spingoOrderProfileFromGenusDf$spingoGenusClassifiedRelativeAbundance <-
    spingoOrderProfileFromGenusDf$spingoGenusRelativeAbundance /
    sum(spingoOrderProfileFromGenusDf$spingoGenusRelativeAbundance, na.rm = TRUE)

  spingoOrderProfileFromSpeciesDf <- rawSpingoSpeciesProfileDf %>%
    group_by(orderId) %>%
    summarize(spingoSpeciesRelativeAbundance = sum(spingoRelativeAbundance)) %>%
    rename(standardId = orderId) %>%
    mutate(rank = "order")
  spingoOrderProfileFromSpeciesDf$spingoSpeciesClassifiedRelativeAbundance <-
    spingoOrderProfileFromSpeciesDf$spingoSpeciesRelativeAbundance /
    sum(spingoOrderProfileFromSpeciesDf$spingoSpeciesRelativeAbundance, na.rm = TRUE)

  spingoOrderProfileDf <- spingoOrderProfileFromGenusDf %>%
    merge(spingoOrderProfileFromSpeciesDf, by = c("rank", "standardId"), all = T)

  ## Class
  spingoClassProfileFromGenusDf <- rawSpingoGenusProfileDf %>%
    group_by(classId) %>%
    summarize(spingoGenusRelativeAbundance = sum(spingoRelativeAbundance)) %>%
    rename(standardId = classId) %>%
    mutate(rank = "class")
  spingoClassProfileFromGenusDf$spingoGenusClassifiedRelativeAbundance <-
    spingoClassProfileFromGenusDf$spingoGenusRelativeAbundance /
    sum(spingoClassProfileFromGenusDf$spingoGenusRelativeAbundance, na.rm = TRUE)

  spingoClassProfileFromSpeciesDf <- rawSpingoSpeciesProfileDf %>%
    group_by(classId) %>%
    summarize(spingoSpeciesRelativeAbundance = sum(spingoRelativeAbundance)) %>%
    rename(standardId = classId) %>%
    mutate(rank = "class")
  spingoClassProfileFromSpeciesDf$spingoSpeciesClassifiedRelativeAbundance <-
    spingoClassProfileFromSpeciesDf$spingoSpeciesRelativeAbundance /
    sum(spingoClassProfileFromSpeciesDf$spingoSpeciesRelativeAbundance, na.rm = TRUE)

  spingoClassProfileDf <- spingoClassProfileFromGenusDf %>%
    merge(spingoClassProfileFromSpeciesDf, by = c("rank", "standardId"), all = T)

  ## Phylum
  spingoPhylumProfileFromGenusDf <- rawSpingoGenusProfileDf %>%
    group_by(phylumId) %>%
    summarize(spingoGenusRelativeAbundance = sum(spingoRelativeAbundance)) %>%
    rename(standardId = phylumId) %>%
    mutate(rank = "phylum")
  spingoPhylumProfileFromGenusDf$spingoGenusClassifiedRelativeAbundance <-
    spingoPhylumProfileFromGenusDf$spingoGenusRelativeAbundance /
    sum(spingoPhylumProfileFromGenusDf$spingoGenusRelativeAbundance, na.rm = TRUE)

  spingoPhylumProfileFromSpeciesDf <- rawSpingoSpeciesProfileDf %>%
    group_by(phylumId) %>%
    summarize(spingoSpeciesRelativeAbundance = sum(spingoRelativeAbundance)) %>%
    rename(standardId = phylumId) %>%
    mutate(rank = "phylum")
  spingoPhylumProfileFromSpeciesDf$spingoSpeciesClassifiedRelativeAbundance <-
    spingoPhylumProfileFromSpeciesDf$spingoSpeciesRelativeAbundance /
    sum(spingoPhylumProfileFromSpeciesDf$spingoSpeciesRelativeAbundance, na.rm = TRUE)

  spingoPhylumProfileDf <- spingoPhylumProfileFromGenusDf %>%
    merge(spingoPhylumProfileFromSpeciesDf, by = c("rank", "standardId"), all = T)

  ## Superkingdom
  spingoSuperkingdomProfileFromGenusDf <- rawSpingoGenusProfileDf %>%
    group_by(superkingdomId) %>%
    summarize(spingoGenusRelativeAbundance = sum(spingoRelativeAbundance)) %>%
    rename(standardId = superkingdomId) %>%
    mutate(rank = "superkingdom")
  spingoSuperkingdomProfileFromGenusDf$spingoGenusClassifiedRelativeAbundance <-
    spingoSuperkingdomProfileFromGenusDf$spingoGenusRelativeAbundance /
    sum(spingoSuperkingdomProfileFromGenusDf$spingoGenusRelativeAbundance, na.rm = TRUE)

  spingoSuperkingdomProfileFromSpeciesDf <- rawSpingoSpeciesProfileDf %>%
    group_by(superkingdomId) %>%
    summarize(spingoSpeciesRelativeAbundance = sum(spingoRelativeAbundance)) %>%
    rename(standardId = superkingdomId) %>%
    mutate(rank = "superkingdom")
  spingoSuperkingdomProfileFromSpeciesDf$spingoSpeciesClassifiedRelativeAbundance <-
    spingoSuperkingdomProfileFromSpeciesDf$spingoSpeciesRelativeAbundance /
    sum(spingoSuperkingdomProfileFromSpeciesDf$spingoSpeciesRelativeAbundance, na.rm = TRUE)

  spingoSuperkingdomProfileDf <- spingoSuperkingdomProfileFromGenusDf %>%
    merge(spingoSuperkingdomProfileFromSpeciesDf, by = c("rank", "standardId"), all = T)

  bindingColumnsNames <- c("rank", "standardId",
                           "spingoGenusRelativeAbundance", "spingoGenusClassifiedRelativeAbundance",
                           "spingoSpeciesRelativeAbundance", "spingoSpeciesClassifiedRelativeAbundance")
  spingoProfileDf <- rbind(
    spingoSuperkingdomProfileDf,
    spingoPhylumProfileDf,
    spingoClassProfileDf,
    spingoOrderProfileDf,
    spingoFamilyProfileDf,
    rawSpingoGenusProfileDf[,bindingColumnsNames],
    rawSpingoSpeciesProfileDf[,bindingColumnsNames]
  ) %>% subset(standardId %>% is.na == F)

  spingoProfileDf$spingoSpeciesRelativeAbundance[spingoProfileDf$spingoSpeciesRelativeAbundance %>% is.na] <- 0
  spingoProfileDf$spingoSpeciesClassifiedRelativeAbundance[spingoProfileDf$spingoSpeciesClassifiedRelativeAbundance %>% is.na] <- 0

  return(spingoProfileDf)
}
