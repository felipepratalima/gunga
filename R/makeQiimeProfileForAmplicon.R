require(taxdumpr)
require(magrittr)
require(stringr)
require(dplyr)

loadQiimeProfile <- function(qiimeProfileLocation = NA) {
  qiimeProfile <- read.table(qiimeProfileLocation, skip = 1, sep = "\t")

  names(qiimeProfile) <- c("lineage", "percentageOfReads")
  qiimeProfile$lineage <- qiimeProfile$lineage %>% as.character

  qiimeProfile$superkingdom <- str_extract(qiimeProfile$lineage, "k__.*?(;|$)")
  qiimeProfile$superkingdom <- str_replace(qiimeProfile$superkingdom, "k__", "")
  qiimeProfile$superkingdom <- str_replace(qiimeProfile$superkingdom, "\\[", "")
  qiimeProfile$superkingdom <- str_replace(qiimeProfile$superkingdom, "\\]", "")
  qiimeProfile$superkingdom <- str_replace(qiimeProfile$superkingdom, ";", "")
  qiimeProfile$superkingdom <- ifelse(
    qiimeProfile$superkingdom != "",
    qiimeProfile$superkingdom,
    NA)

  qiimeProfile$phylum <- str_extract(qiimeProfile$lineage, "p__.*?(;|$)")
  qiimeProfile$phylum <- str_replace(qiimeProfile$phylum, "p__", "")
  qiimeProfile$phylum <- str_replace(qiimeProfile$phylum, "\\[", "")
  qiimeProfile$phylum <- str_replace(qiimeProfile$phylum, "\\]", "")
  qiimeProfile$phylum <- str_replace(qiimeProfile$phylum, ";", "")
  qiimeProfile$phylum <- ifelse(
    qiimeProfile$phylum != "",
    qiimeProfile$phylum,
    NA)

  qiimeProfile$class <- str_extract(qiimeProfile$lineage, "c__.*?(;|$)")
  qiimeProfile$class <- str_replace(qiimeProfile$class, "c__", "")
  qiimeProfile$class <- str_replace(qiimeProfile$class, "\\[", "")
  qiimeProfile$class <- str_replace(qiimeProfile$class, "\\]", "")
  qiimeProfile$class <- str_replace(qiimeProfile$class, ";", "")
  qiimeProfile$class <- ifelse(
    qiimeProfile$class != "",
    qiimeProfile$class,
    NA)

  qiimeProfile$order <- str_extract(qiimeProfile$lineage, "o__.*?(;|$)")
  qiimeProfile$order <- str_replace(qiimeProfile$order, "o__", "")
  qiimeProfile$order <- str_replace(qiimeProfile$order, "\\[", "")
  qiimeProfile$order <- str_replace(qiimeProfile$order, "\\]", "")
  qiimeProfile$order <- str_replace(qiimeProfile$order, ";", "")
  qiimeProfile$order <- ifelse(
    qiimeProfile$order != "",
    qiimeProfile$order,
    NA)

  qiimeProfile$family <- str_extract(qiimeProfile$lineage, "f__.*?(;|$)")
  qiimeProfile$family <- str_replace(qiimeProfile$family, "f__", "")
  qiimeProfile$family <- str_replace(qiimeProfile$family, "\\[", "")
  qiimeProfile$family <- str_replace(qiimeProfile$family, "\\]", "")
  qiimeProfile$family <- str_replace(qiimeProfile$family, ";", "")
  qiimeProfile$family <- ifelse(
    qiimeProfile$family != "",
    qiimeProfile$family,
    NA)

  qiimeProfile$genus <- str_extract(qiimeProfile$lineage, "g__.*?(;|$)")
  qiimeProfile$genus <- str_replace(qiimeProfile$genus, "g__", "")
  qiimeProfile$genus <- str_replace(qiimeProfile$genus, "\\[", "")
  qiimeProfile$genus <- str_replace(qiimeProfile$genus, "\\]", "")
  qiimeProfile$genus <- str_replace(qiimeProfile$genus, ";", "")
  qiimeProfile$genus <- ifelse(
    qiimeProfile$genus != "",
    qiimeProfile$genus,
    NA)

  qiimeProfile$species <- str_extract(qiimeProfile$lineage, "s__.*")
  qiimeProfile$species <- str_replace(qiimeProfile$species, "s__", "")
  qiimeProfile$species <- str_replace(qiimeProfile$species, "\\[", "")
  qiimeProfile$species <- str_replace(qiimeProfile$species, "\\]", "")
  qiimeProfile$species <- ifelse(
    qiimeProfile$species != "",
    paste(qiimeProfile$genus, qiimeProfile$species),
    NA)

  qiimeProfile
}

loadQiimeClassifications <- function(qiimeClassificationsLocation = NA) {
  qiimeClassifications <- read.table(qiimeClassificationsLocation, header = F, sep = "\t", fill = T)

  names(qiimeClassifications) <- c("otuId", "lineage", "score", "numberOfReads")

  qiimeClassificationsLocation <- qiimeClassifications %>% filter(numberOfReads > 1)

  qiimeClassifications$lineage <- qiimeClassifications$lineage %>% as.character

  qiimeClassifications$superkingdom <- str_extract(qiimeClassifications$lineage, "k__.*?(;|$)")
  qiimeClassifications$superkingdom <- str_replace(qiimeClassifications$superkingdom, "k__", "")
  qiimeClassifications$superkingdom <- str_replace(qiimeClassifications$superkingdom, "\\[", "")
  qiimeClassifications$superkingdom <- str_replace(qiimeClassifications$superkingdom, "\\]", "")
  qiimeClassifications$superkingdom <- str_replace(qiimeClassifications$superkingdom, ";", "")
  qiimeClassifications$superkingdom <- ifelse(
    qiimeClassifications$superkingdom != "",
    qiimeClassifications$superkingdom,
    NA)

  qiimeClassifications$phylum <- str_extract(qiimeClassifications$lineage, "p__.*?(;|$)")
  qiimeClassifications$phylum <- str_replace(qiimeClassifications$phylum, "p__", "")
  qiimeClassifications$phylum <- str_replace(qiimeClassifications$phylum, "\\[", "")
  qiimeClassifications$phylum <- str_replace(qiimeClassifications$phylum, "\\]", "")
  qiimeClassifications$phylum <- str_replace(qiimeClassifications$phylum, ";", "")
  qiimeClassifications$phylum <- ifelse(
    qiimeClassifications$phylum != "",
    qiimeClassifications$phylum,
    NA)

  qiimeClassifications$class <- str_extract(qiimeClassifications$lineage, "c__.*?(;|$)")
  qiimeClassifications$class <- str_replace(qiimeClassifications$class, "c__", "")
  qiimeClassifications$class <- str_replace(qiimeClassifications$class, "\\[", "")
  qiimeClassifications$class <- str_replace(qiimeClassifications$class, "\\]", "")
  qiimeClassifications$class <- str_replace(qiimeClassifications$class, ";", "")
  qiimeClassifications$class <- ifelse(
    qiimeClassifications$class != "",
    qiimeClassifications$class,
    NA)

  qiimeClassifications$order <- str_extract(qiimeClassifications$lineage, "o__.*?(;|$)")
  qiimeClassifications$order <- str_replace(qiimeClassifications$order, "o__", "")
  qiimeClassifications$order <- str_replace(qiimeClassifications$order, "\\[", "")
  qiimeClassifications$order <- str_replace(qiimeClassifications$order, "\\]", "")
  qiimeClassifications$order <- str_replace(qiimeClassifications$order, ";", "")
  qiimeClassifications$order <- ifelse(
    qiimeClassifications$order != "",
    qiimeClassifications$order,
    NA)

  qiimeClassifications$family <- str_extract(qiimeClassifications$lineage, "f__.*?(;|$)")
  qiimeClassifications$family <- str_replace(qiimeClassifications$family, "f__", "")
  qiimeClassifications$family <- str_replace(qiimeClassifications$family, "\\[", "")
  qiimeClassifications$family <- str_replace(qiimeClassifications$family, "\\]", "")
  qiimeClassifications$family <- str_replace(qiimeClassifications$family, ";", "")
  qiimeClassifications$family <- ifelse(
    qiimeClassifications$family != "",
    qiimeClassifications$family,
    NA)

  qiimeClassifications$genus <- str_extract(qiimeClassifications$lineage, "g__.*?(;|$)")
  qiimeClassifications$genus <- str_replace(qiimeClassifications$genus, "g__", "")
  qiimeClassifications$genus <- str_replace(qiimeClassifications$genus, "\\[", "")
  qiimeClassifications$genus <- str_replace(qiimeClassifications$genus, "\\]", "")
  qiimeClassifications$genus <- str_replace(qiimeClassifications$genus, ";", "")
  qiimeClassifications$genus <- ifelse(
    qiimeClassifications$genus != "",
    qiimeClassifications$genus,
    NA)

  qiimeClassifications$species <- str_extract(qiimeClassifications$lineage, "s__.*")
  qiimeClassifications$species <- str_replace(qiimeClassifications$species, "s__", "")
  qiimeClassifications$species <- str_replace(qiimeClassifications$species, "\\[", "")
  qiimeClassifications$species <- str_replace(qiimeClassifications$species, "\\]", "")
  qiimeClassifications$species <- ifelse(
    qiimeClassifications$species != "",
    paste(qiimeClassifications$genus, qiimeClassifications$species),
    NA)

  qiimeClassifications
}


makeQiimeProfileForAmplicon <- function(rawQiimeProfileLocation = NULL, rawQiimeClassificationsLocation = NULL, taxdumprObject = NULL) {
  qiimeProfile <- loadQiimeProfile(rawQiimeProfileLocation)
  qiimeProfile$genusId <- getStandardTaxonomyIdsByNames(taxdumprObject, qiimeProfile$genus)
  qiimeProfile <- qiimeProfile %>% subset(genusId %>% is.na == FALSE)
  qiimeProfile <- qiimeProfile %>% group_by(genusId) %>% summarise(qiimePercentual = sum(percentageOfReads))

  qiimeClassifications <- loadQiimeClassifications(rawQiimeClassificationsLocation)
  qiimeClassifications$genusId <- getStandardTaxonomyIdsByNames(taxdumprObject, qiimeClassifications$genus)
  qiimeClassifications <- qiimeClassifications %>% subset(genusId %>% is.na == FALSE)

  otusNumbers <- qiimeClassifications %>% group_by(genusId) %>% summarise(qiimeOtusNumber = n())
  qiimeProfile <- merge(qiimeProfile, otusNumbers, by = "genusId")

  qiimeProfileForAmp <- qiimeProfile %>% select(genusId, qiimePercentual, qiimeOtusNumber)

  qiimeProfileForAmp$qiimeAmp <- TRUE

  return(qiimeProfileForAmp)
}

# qiimeGungaProfileDf <- makeQiimeProfileForAmplicon("data-raw/qiime/open_reference_otus/taxa_summary/otu_table_mc2_w_tax_L6.txt",
#                                                    "data-raw/qiime/open_reference_otus/uclust_assigned_taxonomy/rep_set_tax_assignments.txt",
#                                                    taxdumprObject)
