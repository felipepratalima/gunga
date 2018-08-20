library(gunga)
library(magrittr)
library(stringr)
library(dplyr)

context("Qiime: makeQiimeProfileForAmplicon")



taxdumprObject <- Taxdumpr("~/taxdump/nodes.dmp", "~/taxdump/names.dmp")

test_that("Test makeQiimeProfileForAmplicon", {
  expect_equal(1,2)
  # integratedProfile <- mergeProfiles(centrifugeGungaProfileForSh, spingoGungaProfileDf, qiimeGungaProfileDf, taxdumprObject)
  # expect_equal(integratedProfile %>% nrow, 53)
  # expect_equal(integratedProfile %>% ncol, 25)
  # expect_equal(integratedProfile %>% colnames, INTEGRATED_GUNGA_PROFILE_COLUMNS_NAMES)
})
