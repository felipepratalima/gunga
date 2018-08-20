library(gunga)
library(magrittr)
library(stringr)
library(dplyr)

context("Qiime: makeQiimeProfileForAmplicon")

QIIME_PROFILE_LOCATION <- system.file("testdata", "../../data-raw/qiime/open_reference_otus/taxa_summary/otu_table_mc2_w_tax_L6.txt", package = "gunga")
QIIME_CLASSIFICATIONS_LOCATION <- system.file("testdata", "../../data-raw/qiime/open_reference_otus/uclust_assigned_taxonomy/rep_set_tax_assignments.txt", package = "gunga")
taxdumprObject <- Taxdumpr("~/taxdump/nodes.dmp", "~/taxdump/names.dmp")
QIIME_GUNGA_PROFILE_COLUMNS_NAMES <- c("genusId", "qiimePercentual", "qiimeOtusNumber", "qiimeAmp")

test_that("Test makeQiimeProfileForAmplicon", {
  qiimeGungaProfileForAmp <- makeQiimeProfileForAmplicon(QIIME_PROFILE_LOCATION, QIIME_CLASSIFICATIONS_LOCATION, taxdumprObject)
  expect_equal(qiimeGungaProfileForAmp %>% nrow, 53)
  expect_equal(qiimeGungaProfileForAmp %>% ncol, 4)
  expect_equal(qiimeGungaProfileForAmp %>% colnames, QIIME_GUNGA_PROFILE_COLUMNS_NAMES)
})
