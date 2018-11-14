library(magrittr)
library(stringr)
library(dplyr)
library(readr)
library(taxdumpr)

## Clark output data location
# clarkLocation <- "~/projects/gunga/data-raw/sh/clark_classifications.csv"

args = commandArgs(trailingOnly=TRUE)
clarkLocation <- args[1]

## Load Bowtie2 data
clarkDf <- read_csv(clarkLocation, col_names = T)
clarkDf$Length <- NULL

## Define output file
outputFile <- dirname(clarkLocation) %>% paste0("/wevote_clark.tsv")
print("Generating output to " %>% paste0(outputFile))

## Export data
write.table(clarkDf, outputFile, quote = F, sep = "\t", row.names = F, col.names = F)
