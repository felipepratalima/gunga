library(gunga)
library(magrittr)
library(stringr)
library(dplyr)
library(taxdumpr)

context("Spingo: makeSpingoProfileForAmplicon")

SPINGO_PROFILE_LOCATION <- system.file("extdata", "../../data-raw/spingo/seqs.species.out", package = "gunga")
SPINGO_CLASSIFICATIONS_LOCATION <- system.file("extdata", "../../data-raw/spingo/seqs.results.out", package = "gunga")
taxdumprObject <- Taxdumpr("~/taxdump/nodes.dmp", "~/taxdump/names.dmp", "~/taxdump/merged.dmp")
SPINGO_GUNGA_PROFILE_COLUMNS_NAMES <- c("speciesId", "spingoPercentual", "spingoScoreMedian", "spingoScoreCI", "spingoAmp")

test_that("Test makeSpingoProfileForAmplicon", {
  spingoGungaProfileForAmp <- makeSpingoProfileForAmplicon(SPINGO_PROFILE_LOCATION, SPINGO_CLASSIFICATIONS_LOCATION, taxdumprObject)
  expect_equal(spingoGungaProfileForAmp %>% nrow, 87)
  expect_equal(spingoGungaProfileForAmp %>% ncol, 5)
  expect_equal(spingoGungaProfileForAmp %>% colnames, SPINGO_GUNGA_PROFILE_COLUMNS_NAMES)
})
