library(magrittr)
library(stringr)
library(dplyr)
library(readr)
library(taxdumpr)

## TIPP output data location
# wevoteLocation <- "~/projects/gunga/data-raw/sh/"

args = commandArgs(trailingOnly=TRUE)
wevoteLocation <- args[1]

clarkLocation <- wevoteLocation %>% paste0("/wevote_clark.tsv")
krakenLocation <- wevoteLocation %>% paste0("/wevote_kraken.tsv")
meganLocation <- wevoteLocation %>% paste0("/wevote_megan.tsv")

mergedDf <- NULL

if (file.exists(clarkLocation)) {
  currentDf <- read_tsv(clarkLocation, col_names = c("seq", "clark"))

  if (mergedDf %>% is.null) {
    mergedDf <- currentDf
  } else {
    mergedDf <- mergedDf %>% merge(currentDf, all =T, by = "seq")
  }
}

if (file.exists(krakenLocation)) {
  currentDf <- read_tsv(krakenLocation, col_names = c("seq", "kraken"))

  if (mergedDf %>% is.null) {
    mergedDf <- currentDf
  } else {
    mergedDf <- mergedDf %>% merge(currentDf, all =T, by = "seq")
  }
}

if (file.exists(meganLocation)) {
  currentDf <- read_tsv(meganLocation, col_names = c("seq", "megan"))

  if (mergedDf %>% is.null) {
    mergedDf <- currentDf
  } else {
    mergedDf <- mergedDf %>% merge(currentDf, all =T, by = "seq")
  }
}

head(mergedDf)

for(columnName in colnames(mergedDf)) {
  naIndexes <- mergedDf[,columnName] %>% is.na
  mergedDf[naIndexes,columnName] <- 0
}

## Define output file
outputFile <- wevoteLocation %>% paste0("/wevote3_joined.csv")
print("Generating output to " %>% paste0(outputFile))

## Export data
write.table(mergedDf, outputFile, quote = F, sep = ",", row.names = F, col.names = F)
