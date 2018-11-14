library(magrittr)
library(stringr)
library(dplyr)
library(readr)
library(taxdumpr)

## Megan output data location
# meganLocation <- "~/projects/gunga/data-raw/sh/besthit_megan_classifications.tsv"

args = commandArgs(trailingOnly=TRUE)
meganLocation <- args[1]

## Load Bowtie2 data
meganDf <- read_table(meganLocation, col_names = c("description"))
meganDf$seq <- meganDf$description %>% str_extract("^.*?;") %>% str_replace(";", "")
meganDf$taxid <- meganDf$description %>% str_extract("s__[0-9]*") %>% str_replace("s__", "")
meganDf$taxid[meganDf$taxid %>% is.na] <- meganDf$description[meganDf$taxid %>% is.na] %>% str_extract("g__[0-9]*") %>% str_replace("g__", "")
meganDf$taxid[meganDf$taxid %>% is.na] <- meganDf$description[meganDf$taxid %>% is.na] %>% str_extract("f__[0-9]*") %>% str_replace("f__", "")
meganDf$taxid[meganDf$taxid %>% is.na] <- meganDf$description[meganDf$taxid %>% is.na] %>% str_extract("o__[0-9]*") %>% str_replace("o__", "")
meganDf$taxid[meganDf$taxid %>% is.na] <- meganDf$description[meganDf$taxid %>% is.na] %>% str_extract("c__[0-9]*") %>% str_replace("c__", "")
meganDf$taxid[meganDf$taxid %>% is.na] <- meganDf$description[meganDf$taxid %>% is.na] %>% str_extract("p__[0-9]*") %>% str_replace("p__", "")
meganDf$taxid[meganDf$taxid %>% is.na] <- meganDf$description[meganDf$taxid %>% is.na] %>% str_extract("d__[0-9]*") %>% str_replace("d__", "")
meganDf$taxid[meganDf$taxid %>% is.na] <- 0
meganDf$description <- NULL


## Define output file
outputFile <- dirname(meganLocation) %>% paste0("/wevote_megan.tsv")
print("Generating output to " %>% paste0(outputFile))

## Export data
write.table(meganDf, outputFile, quote = F, sep = "\t", row.names = F, col.names = F)
