library(gunga)
library(magrittr)
library(stringr)
library(dplyr)

context("Centrifuge: makeCentrifugeProfileForShotgun")

## Constants
CENTRIFUGE_CLASSIFICATIONS_LOCATION <- system.file("testdata", "../../data-raw/sh/centrifuge_classifications.tsv", package = "gunga")

CENTRIFUGE_PROFILE_LOCATION <- system.file("testdata", "../../data-raw/sh/centrifuge_report.tsv", package = "gunga")

CENTRIFUGE_GUNGA_PROFILE_COLUMNS_NAMES <- c("speciesId",
                                            "dnaCentrifugePercentual",
                                            "dnaCentrifugeScoreMedian","dnaCentrifugeScoreCI",
                                            "dnaCentrifugeHitLengthMedian","dnaCentrifugeHitLengthCI",
                                            "dnaCentrifugeQueryLengthMedian","dnaCentrifugeQueryLengthCI",
                                            "dnaCentrifugeNumberOfMatchesMedian", "dnaCentrifugeNumberOfMatchesCI",
                                            "centrifugeSh")

rawCentrifugeClassificationsDf <- loadRawCentrifugeClassifications(CENTRIFUGE_CLASSIFICATIONS_LOCATION)

rawCentrifugeProfileDf <- loadRawCentrifugeProfile(CENTRIFUGE_PROFILE_LOCATION)

taxdumprObject <- Taxdumpr("~/taxdump/nodes.dmp", "~/taxdump/names.dmp")

test_that("Test makeCentrifugeProfileForShotgun with rawCentrifugeProfileDf parameter null", {
  expect_error(makeCentrifugeProfileForShotgun(rawCentrifugeProfileDf = NULL, rawCentrifugeClassificationsDf = NULL, taxdumprObject = NULL),
               "The rawCentrifugeProfileDf parameter should not be null")
})

test_that("Test makeCentrifugeProfileForShotgun with rawCentrifugeProfileDf parameter NA", {
  expect_error(makeCentrifugeProfileForShotgun(rawCentrifugeProfileDf = NA, rawCentrifugeClassificationsDf = NULL, taxdumprObject = NULL),
               "The rawCentrifugeProfileDf parameter should not be NA")
})

test_that("Test makeCentrifugeProfileForShotgun with rawCentrifugeClassificationsDf parameter null", {
  expect_error(makeCentrifugeProfileForShotgun(rawCentrifugeProfileDf = rawCentrifugeProfileDf, rawCentrifugeClassificationsDf = NULL, taxdumprObject = NULL),
               "The rawCentrifugeClassificationsDf parameter should not be null")
})

test_that("Test makeCentrifugeProfileForShotgun with rawCentrifugeClassificationsDf parameter NA", {
  expect_error(makeCentrifugeProfileForShotgun(rawCentrifugeProfileDf = rawCentrifugeProfileDf, rawCentrifugeClassificationsDf = NA, taxdumprObject = NULL),
               "The rawCentrifugeClassificationsDf parameter should not be NA")
})

test_that("Test makeCentrifugeProfileForShotgun with taxdumprObject parameter null", {
  expect_error(makeCentrifugeProfileForShotgun(rawCentrifugeProfileDf = rawCentrifugeProfileDf, rawCentrifugeClassificationsDf = rawCentrifugeClassificationsDf, taxdumprObject = NULL),
               "The taxdumprObject parameter should not be null")
})

test_that("Test makeCentrifugeProfileForShotgun with taxdumprObject parameter NA", {
  expect_error(makeCentrifugeProfileForShotgun(rawCentrifugeProfileDf = rawCentrifugeProfileDf, rawCentrifugeClassificationsDf = rawCentrifugeClassificationsDf, taxdumprObject = NA),
               "The taxdumprObject parameter should not be NA")
})

test_that("Test makeCentrifugeProfileForShotgun", {
  centrifugeGungaProfileForSh <- makeCentrifugeProfileForShotgun(rawCentrifugeProfileDf, rawCentrifugeClassificationsDf, taxdumprObject)
  expect_equal(centrifugeGungaProfileForSh %>% nrow, 1156)
  expect_equal(centrifugeGungaProfileForSh %>% ncol, 11)
  expect_equal(centrifugeGungaProfileForSh %>% colnames, CENTRIFUGE_GUNGA_PROFILE_COLUMNS_NAMES)
})
