library(methods)
library(magrittr)
library(stringr)
library(dplyr)
library(readr)
library(taxdumpr)

## Load Taxdumpr
x <- Taxdumpr("~/taxdump/nodes.dmp", "~/taxdump/names.dmp", "~/taxdump/merged.dmp")

## Load Metaphlan Data
markerInfoLocation <- "mpa_v20_m200_marker_info.txt.bz2"
markerInfoDf <- read_tsv(markerInfoLocation, col_names = F)
colnames(markerInfoDf) <- c("id", "description")

## Get Clade Name
markerInfoDf$cladeTaxonomyName <-
  markerInfoDf$description %>%
  str_extract("'clade': '.*?'") %>%
  str_extract("'[a-z]__.*?'") %>%
  str_replace_all("'", "") %>%
  str_replace("[a-z]__", "") %>%
  str_replace_all("_", " ") %>%
  str_replace_all(" sp ", " sp. ") %>%
  str_replace_all("GCF ", "GCF_")

## Get Taxonomy Name
markerInfoDf$taxonomyName <-
  markerInfoDf$description %>%
  str_extract("'taxon': '.*?'") %>%
  str_replace_all("\\|t__.*", "") %>%
  str_replace_all(".*\\|", "") %>%
  str_replace_all("[']", "") %>%
  str_replace_all(",", "") %>%
  str_replace_all("[kpcofgs]__", "") %>%
  str_replace_all("_", " ") %>%
  str_replace_all(" sp ", " sp. ") %>%
  str_replace_all(" sp$", " sp.$") %>%
  str_replace_all("taxon: ", "") %>%
  str_replace(" noname", "") %>%
  str_replace(" genomosp ", " genomosp. ") %>%
  str_replace(" Incertae Sedis", ". Incertae Sedis") %>%
  str_replace("Candidatus Liberibacter$", "Liberibacter")


## Problem: Bacillus cereus thuringiensis (Not NCBI compatible)
## -> Bacillus cereus or
## -> Bacillus thuringiensis

## Process taxonomy with taxdumpr
markerInfoDf$taxonomyId <- markerInfoDf$taxonomyName %>% getStandardTaxonomyIdsByNames(x, .)
markerInfoDf$taxonomyId[markerInfoDf$cladeTaxonomyName == "Lactate dehydrogenase elevating virus"] <- 11048

## Split info
readyMarkerInfoDf <- markerInfoDf %>% subset(taxonomyId %>% is.na == F)

## Complete unready info
unreadyMarkerInfoDf <- markerInfoDf %>% subset(taxonomyId %>% is.na == T)
unreadyMarkerInfoDf$taxonomyName <-
  unreadyMarkerInfoDf$description %>%
  str_extract("g__.*?('|\\|)") %>%
  str_replace_all("\\|", "") %>%
  # str_replace_all(".*\\|", "") %>%
  str_replace_all("[']", "") %>%
  str_replace_all(",", "") %>%
  str_replace_all("g__", "") %>%
  str_replace_all("_", " ") %>%
  str_replace_all(" sp ", " sp. ") %>%
  str_replace_all(" sp$", " sp.$") %>%
  str_replace_all("taxon: ", "") %>%
  str_replace(" noname", "") %>%
  str_replace(" genomosp ", " genomosp. ") %>%
  str_replace(" Incertae Sedis", ". Incertae Sedis") %>%
  str_replace("Candidatus Liberibacter$", "Liberibacter")
unreadyMarkerInfoDf$taxonomyId <- unreadyMarkerInfoDf$taxonomyName %>% getStandardTaxonomyIdsByNames(x, .)

## Bind completed info
finalMarkerInfoDf <- rbind(readyMarkerInfoDf, unreadyMarkerInfoDf)

## Make map
  metaphlan2MapDf <- finalMarkerInfoDf %>% select(ref = id, taxid = taxonomyId)

## Export file
write.table(metaphlan2MapDf, "mpa_v20_m200_marker_info.tsv", sep = "\t", quote = F, row.names = F, col.names = T)
