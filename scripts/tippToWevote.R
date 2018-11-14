library(magrittr)
library(stringr)
library(dplyr)
library(readr)
library(taxdumpr)

## TIPP output data location
# tippLocation <- "~/projects/gunga/data-raw/sh/tipp_profile/markers/all.classification"

args = commandArgs(trailingOnly=TRUE)
tippLocation <- args[1]

## Load Bowtie2 data
tippDf <- read_tsv(tippLocation, col_names = T)
tippDf$taxonomyId <-tippDf$species
tippDf$taxonomyId[tippDf$taxonomyId %>% is.na] <- tippDf$genus[tippDf$taxonomyId %>% is.na]
tippDf$taxonomyId[tippDf$taxonomyId %>% is.na] <- tippDf$family[tippDf$taxonomyId %>% is.na]
tippDf$taxonomyId[tippDf$taxonomyId %>% is.na] <- tippDf$order[tippDf$taxonomyId %>% is.na]
tippDf$taxonomyId[tippDf$taxonomyId %>% is.na] <- tippDf$class[tippDf$taxonomyId %>% is.na]
tippDf$taxonomyId[tippDf$taxonomyId %>% is.na] <- tippDf$phylum[tippDf$taxonomyId %>% is.na]
tippDf$taxonomyId[tippDf$taxonomyId %>% is.na] <- 0
tippDf <- tippDf %>% select(fragment, taxonomyId)

## Define output file
outputFile <- dirname(tippLocation) %>% paste0("/../../wevote_tipp.tsv")
print("Generating output to " %>% paste0(outputFile))

## Export data
write.table(tippDf, outputFile, quote = F, sep = "\t", row.names = F, col.names = F)
