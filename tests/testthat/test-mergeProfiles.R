library(gunga)
library(magrittr)
library(stringr)
library(dplyr)
library(taxdumpr)

context("Merging: mergeProfiles")



taxdumprObject <- Taxdumpr("~/taxdump/nodes.dmp", "~/taxdump/names.dmp", "~/taxdump/merged.dmp")

test_that("Test mergeProfiles", {
  throws_error("Not implemented yet")
  # integratedProfile <- mergeProfiles(centrifugeGungaProfileForSh, spingoGungaProfileDf, qiimeGungaProfileDf, taxdumprObject)
  # expect_equal(integratedProfile %>% nrow, 53)
  # expect_equal(integratedProfile %>% ncol, 25)
  # expect_equal(integratedProfile %>% colnames, INTEGRATED_GUNGA_PROFILE_COLUMNS_NAMES)
})
