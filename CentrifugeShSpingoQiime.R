#' ---
#' title: "gunga: Centrifuge (Shotgun) + Spingo + QIIME"
#' output: github_document
#' ---
knitr::opts_chunk$set(echo = TRUE)

#' ## Load necessary packages
require(magrittr)
require(dplyr)
require(stringr)
require(taxdumpr)
require(gunga)
require(randomForest)
require(ggplot2)
require(scatterplot3d)
require(caret)

#' ## Instantiate the **Taxdumpr** base class

taxdumprObject <- Taxdumpr(nodesDmpLocation = "~/taxdump/nodes.dmp", namesDmpLocation = "~/taxdump/names.dmp")


#' ## Load the Centrifuge results
#'
rawCentrifugeProfileDf <- loadRawCentrifugeProfile("data-raw/sh/centrifuge_report.tsv")
head(rawCentrifugeProfileDf)
rawCentrifugeClassificationsDf <- loadRawCentrifugeClassifications("data-raw/sh/centrifuge_classifications.tsv")
head(rawCentrifugeClassificationsDf)
centrifugeProfileForShotgunDf <- makeCentrifugeProfileForShotgun(rawCentrifugeProfileDf, rawCentrifugeClassificationsDf, taxdumprObject)
head(centrifugeProfileForShotgunDf)


#' ## Load the Spingo results
#'
spingoProfileDf <- makeSpingoProfileForAmplicon("data-raw/spingo/seqs.species.out",
                                                "data-raw/spingo/seqs.results.out",
                                                taxdumprObject)
head(spingoProfileDf)


#' ## Load the QIIME results
#'
qiimeProfileDf <- makeQiimeProfileForAmplicon("data-raw/qiime/open_reference_otus/taxa_summary/otu_table_mc2_w_tax_L6.txt",
                                              "data-raw/qiime/open_reference_otus/uclust_assigned_taxonomy/rep_set_tax_assignments.txt",
                                              taxdumprObject)
head(qiimeProfileDf)


#' ## Merge profiles
integratedProfileDf <- mergeProfiles(centrifugeGungaProfileForSh = centrifugeProfileForShotgunDf, spingoGungaProfileDf = spingoProfileDf, qiimeGungaProfileDf = qiimeProfileDf, taxdumprObject = taxdumprObject)
head(integratedProfileDf)


#' ## Classify True and False Positives
# rf.fit <- readRDS("data/CentrifugeSh_Spingo_Qiime_RF_ntree1000.rds")
# integratedProfilePredictions <- predict(rf.fit, integratedProfileDf)
# integratedProfileDf$classification <- integratedProfilePredictions == "TRUE"
integratedProfileDf <- clasifyCentrifugeShotgunSpingoAndQiimeGungaProfile(integratedProfileDf)


#' ## True and False Positivies classifications
table(integratedProfileDf$classification)


#' ## Variable importance selection to plot
rf.fit <- readRDS("data/CentrifugeSh_Spingo_Qiime_RF_ntree1000.rds")
rfImportanceDf <- rf.fit$importance %>% data.frame
rfImportanceDf$variableName <- rf.fit$importance %>% data.frame %>% rownames
rfImportanceDf <- rfImportanceDf %>% arrange(desc(MeanDecreaseAccuracy))


#' ## Accuracy based:
rfImportanceDf$variableName <- factor(rfImportanceDf$variableName, levels = rfImportanceDf$variableName[order(rfImportanceDf$MeanDecreaseAccuracy)])
p <-
  rfImportanceDf %>%
  ggplot(aes(x = variableName, y = MeanDecreaseGini)) +
  geom_bar(stat='identity', position='dodge') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Mean Decrease Gini") + ylab("Variável") +
  coord_flip()
p


#' ## Gini based:
rfImportanceDf$variableName <- factor(rfImportanceDf$variableName, levels = rfImportanceDf$variableName[order(rfImportanceDf$MeanDecreaseGini)])
p <-
  rfImportanceDf %>%
  ggplot(aes(x = variableName, y = MeanDecreaseGini)) +
  geom_bar(stat='identity', position='dodge') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Mean Decrease Gini") + ylab("Variável") +
  coord_flip()
p

#' ## Basic plot with 2 main features according to random forest importante (Accuracy)
integratedProfileDf %>%
  ggplot(aes(dnaCentrifugePercentual, spingoPercentual, color = classification)) +
  geom_point() +
  theme(legend.position = "top")

#' ## And with the 3 main features (Accuracy)
scatterplot3d(integratedProfileDf %>% select(dnaCentrifugePercentual, spingoPercentual, spingoScoreMedian),
              pch = 16,
              color= ifelse(integratedProfileDf$classification, "#7799cc", "#cc9977"))

#' ## Basic plot with 2 main features according to random forest importante (Gini)
integratedProfileDf %>%
  ggplot(aes(dnaCentrifugeScoreMedian, spingoPercentual, color = classification)) +
  geom_point() +
  theme(legend.position = "top")

#' ## And with the 3 main features (Gini)
scatterplot3d(integratedProfileDf %>% select(dnaCentrifugeScoreMedian, spingoPercentual, dnaCentrifugePercentual),
              pch = 16,
              color= ifelse(integratedProfileDf$classification, "#7799cc", "#cc9977"))

#' ## Performance evaluation
#'
#' Only beacuse we know the truth about our data - in general, users don't knkow.
truthSetCompleteTaxonomy <- read.delim("inst/extdata/truth_set_complete_taxonomies.tsv") %>% subset(datasetName == "Mix1")
integratedProfileDf$verification <- integratedProfileDf$speciesId %in% truthSetCompleteTaxonomy$speciesId

#' ### RF:
#'
integratedProfileDf %$% confusionMatrix(classification %>% factor, verification %>% factor)

#' ### Centrifuge:
#'
integratedProfileDf %>% subset(centrifugeSh) %$% confusionMatrix(centrifugeSh %>% factor, verification %>% factor)

#' ### Spingo:
#'
integratedProfileDf %>% subset(spingoAmp) %$% confusionMatrix(spingoAmp %>% factor, verification %>% factor)



#' ## A shorter version for all
#'
# integratedProfileDf <- gungaWorkflowForCentrifugeShotgunSpingoAndQiime("data-raw/sh/centrifuge_report.tsv", "data-raw/sh/centrifuge_classifications.tsv",
#                                                                        "data-raw/spingo/seqs.species.out", "data-raw/spingo/seqs.results.out",
#                                                                        "data-raw/qiime/open_reference_otus/taxa_summary/otu_table_mc2_w_tax_L6.txt",
#                                                                        "data-raw/qiime/open_reference_otus/uclust_assigned_taxonomy/rep_set_tax_assignments.txt",
#                                                                        "~/taxdump/nodes.dmp", "~/taxdump/names.dmp")
