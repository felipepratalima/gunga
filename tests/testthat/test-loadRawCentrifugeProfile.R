library(gunga)
library(magrittr)
library(stringr)
library(dplyr)

context("Centrifuge: loadRawCentrifugeProfile")

## Constants
CENTRIFUGE_PROFILE_LOCATION <- system.file("testdata", "../../data-raw/sh/centrifuge_report.tsv", package = "gunga")
CENTRIFUGE_PROFILE_COLUMNS_NAMES <- c("name","taxID","taxRank","genomeSize","numReads","numUniqueReads","abundance")

test_that("Test loadRawCentrifugeProfile with centrifugeClassificationsLocation parameter null", {
  expect_error(loadRawCentrifugeClassifications(NULL), "The centrifugeClassificationsLocation parameter should not be null")
})

test_that("Test loadRawCentrifugeProfile with centrifugeClassificationsLocation parameter NA", {
  expect_error(loadRawCentrifugeClassifications(NA), "The centrifugeClassificationsLocation parameter should not be NA")
})

test_that("Test loadRawCentrifugeProfile with centrifugeClassificationsLocation parameter empty", {
  expect_error(loadRawCentrifugeClassifications(""), "The centrifugeClassificationsLocation parameter should not be empty")
})

test_that("Test loadRawCentrifugeProfile with centrifugeClassificationsLocation parameter invalid (inexistent file)", {
  expect_error(loadRawCentrifugeClassifications("inexistentFile.tsv"), "The centrifugeClassificationsLocation parameter should not be an inexistent file")
})

test_that("Test loadRawCentrifugeProfile", {
  rawCentrifugeProfileDf <- loadRawCentrifugeProfile(CENTRIFUGE_PROFILE_LOCATION)
  expect_equal(rawCentrifugeProfileDf %>% nrow, 1812)
  expect_equal(rawCentrifugeProfileDf %>% ncol, 7)
  expect_equal(rawCentrifugeProfileDf %>% colnames, CENTRIFUGE_PROFILE_COLUMNS_NAMES)
})
