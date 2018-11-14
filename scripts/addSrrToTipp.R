library(magrittr)
library(stringr)
library(dplyr)
library(readr)
library(taxdumpr)

## TIPP output data location
# tippLocation <- "~/projects/gunga/data-raw/sh/wevote_tipp.tsv"

args = commandArgs(trailingOnly=TRUE)
tippLocation <- args[1]
srrDescription <- args[2]

## Load Bowtie2 data
tippDf <- read_tsv(tippLocation, col_names = F)
tippDf$X1 <- srrDescription %>% paste0(".") %>% paste0(tippDf$X1) %>% paste0(".1")

## Export data
write.table(tippDf, tippLocation, quote = F, sep = "\t", row.names = F, col.names = F)
