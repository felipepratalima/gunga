library(gunga)
library(magrittr)
library(stringr)
library(dplyr)

context("Centrifuge: loadRawCentrifugeClassificaions")

## Constants
CENTRIFUGE_CLASSIFICATIONS_LOCATION <- system.file("extdata", "../../data-raw/sh/centrifuge_classifications.tsv", package = "gunga")
CENTRIFUGE_CLASSIFICATIONS_LOCATION <- system.file("extdata", "../../data-raw/sh/centrifuge_classifications.tsv", package = "gunga")
CENTRIFUGE_CLASSIFICATIONS_COLUMNS_NAMES <- c("readID","seqID","taxID","score","X2ndBestScore","hitLength","queryLength","numMatches")

test_that("Test loadRawCentrifugeClassificaions with centrifugeClassificationsLocation parameter null", {
  expect_error(loadRawCentrifugeClassifications(NULL), "The centrifugeClassificationsLocation parameter should not be null")
})

test_that("Test loadRawCentrifugeClassificaions with centrifugeClassificationsLocation parameter NA", {
  expect_error(loadRawCentrifugeClassifications(NA), "The centrifugeClassificationsLocation parameter should not be NA")
})

test_that("Test loadRawCentrifugeClassificaions with centrifugeClassificationsLocation parameter empty", {
  expect_error(loadRawCentrifugeClassifications(""), "The centrifugeClassificationsLocation parameter should not be empty")
})

test_that("Test loadRawCentrifugeClassificaions with centrifugeClassificationsLocation parameter invalid (inexistent file)", {
  expect_error(loadRawCentrifugeClassifications("inexistentFile.tsv"), "The centrifugeClassificationsLocation parameter should not be an inexistent file")
})

test_that("Test loadRawCentrifugeClassificaions", {
  rawCentrifugeClassifications <- loadRawCentrifugeClassifications(CENTRIFUGE_CLASSIFICATIONS_LOCATION)
  expect_equal(rawCentrifugeClassifications %>% nrow, 1561161)
  expect_equal(rawCentrifugeClassifications %>% ncol, 8)
  expect_equal(rawCentrifugeClassifications %>% colnames, CENTRIFUGE_CLASSIFICATIONS_COLUMNS_NAMES)
})
