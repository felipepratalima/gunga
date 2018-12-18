library(gunga)
library(magrittr)
library(stringr)
library(dplyr)

context("Centrifuge: loadRawCentrifugeProfile")

## Constants
CENTRIFUGE_PROFILE_LOCATION <- system.file("extdata", "../../data-raw/sh/centrifuge_report.tsv", package = "gunga")
CENTRIFUGE_PROFILE_COLUMNS_NAMES <- c("name","taxID","taxRank","genomeSize","numReads","numUniqueReads","abundance")

test_that("Test loadRawCentrifugeProfile with centrifugeProfileLocation parameter null", {
  expect_error(loadRawCentrifugeProfile(NULL), "The centrifugeProfileLocation parameter should not be null")
})

test_that("Test loadRawCentrifugeProfile with centrifugeProfileLocation parameter NA", {
  expect_error(loadRawCentrifugeProfile(NA), "The centrifugeProfileLocation parameter should not be NA")
})

test_that("Test loadRawCentrifugeProfile with centrifugeProfileLocation parameter empty", {
  expect_error(loadRawCentrifugeProfile(""), "The centrifugeProfileLocation parameter should not be empty")
})

test_that("Test loadRawCentrifugeProfile with centrifugeProfileLocation parameter invalid (inexistent file)", {
  expect_error(loadRawCentrifugeProfile("inexistentFile.tsv"), "The centrifugeProfileLocation parameter should not be an inexistent file")
})

test_that("Test loadRawCentrifugeProfile", {
  rawCentrifugeProfileDf <- loadRawCentrifugeProfile(CENTRIFUGE_PROFILE_LOCATION)
  expect_equal(rawCentrifugeProfileDf %>% nrow, 1812)
  expect_equal(rawCentrifugeProfileDf %>% ncol, 7)
  expect_equal(rawCentrifugeProfileDf %>% colnames, CENTRIFUGE_PROFILE_COLUMNS_NAMES)
})
