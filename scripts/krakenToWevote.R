library(magrittr)
library(stringr)
library(dplyr)
library(readr)
library(taxdumpr)

## Kraken output data location
# krakenLocation <- "~/projects/gunga/data-raw/sh/kraken_classifications.tsv"

args = commandArgs(trailingOnly=TRUE)
krakenLocation <- args[1]

## Load Bowtie2 data
krakenDf <- read_tsv(krakenLocation, col_names = F)
colnames(krakenDf) <- c("status", "sequenceId", "taxonomyId", "length", "lca")
krakenDf$length <- NULL
krakenDf$lca <- NULL
# krakenDf$taxonomyId[krakenDf$status == "U"] <- NA
krakenDf$status <- NULL

## Define output file
outputFile <- dirname(krakenLocation) %>% paste0("/wevote_kraken.tsv")
print("Generating output to " %>% paste0(outputFile))

## Export data
write.table(krakenDf, outputFile, quote = F, sep = "\t", row.names = F, col.names = F)
