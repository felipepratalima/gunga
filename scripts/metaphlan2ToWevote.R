library(magrittr)
library(stringr)
library(dplyr)
library(readr)
library(taxdumpr)

## Botwtie2 output data location for MetaPhlAn2
# bowtie2Location <- "~/projects/gunga/data-raw/sh/metaphlan2.bowtie"

args = commandArgs(trailingOnly=TRUE)
bowtie2Location <- args[1]

## Load Bowtie2 data
bowtie2Df <- read_tsv(bowtie2Location, col_names = F)
colnames(bowtie2Df) <- c("sequenceId", "ref")

bowtie2Df$sequenceId <- bowtie2Df$sequenceId %>% str_replace("_[0-9]*_length=[0-9]*", "")

## Get marker info data
markerInfoDf <- read_tsv("mpa_v20_m200_marker_info.tsv", col_names = T)

## Merge
mergedDf <- bowtie2Df %>%
  merge(markerInfoDf, all.x = T, all.y = F, by = "ref")

## Define output file
outputFile <- dirname(bowtie2Location) %>% paste0("/wevote_metaphlan2.tsv")
print("Generating output to " %>% paste0(outputFile))

## Export data
write.table(mergedDf %>% select(sequenceId, taxid), outputFile, quote = F, sep = "\t", row.names = F, col.names = F)
