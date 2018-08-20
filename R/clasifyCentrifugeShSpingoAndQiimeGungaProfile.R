require(magrittr)
require(dplyr)
require(stringr)
require(taxdumpr)
require(randomForest)

clasifyCentrifugeShotgunSpingoAndQiimeGungaProfile <- function(gungaProfile = NULL) {
  rf.fit <- readRDS(path.package("gunga") %>% paste0("/data/CentrifugeSh_Spingo_Qiime_RF_ntree1000.rds"))
  integratedProfilePredictions <- predict(rf.fit, gungaProfile)
  gungaProfile$classification <- integratedProfilePredictions == "TRUE"
  return(gungaProfile)
}

gungaWorkflowForCentrifugeShotgunSpingoAndQiime <- function(rawCentrifugeProfileLocation = NULL,
                                                            rawCentrifugeClassificationsLocation = NULL,
                                                            rawSpingoProfileLocation = NULL,
                                                            rawSpingoProfileClassifications = NULL,
                                                            rawQiimeProfileLocation = NULL,
                                                            rawQiimeProfileClassifications = NULL,
                                                            nodesDmpLocation = NULL,
                                                            namesDmpLocation = NULL) {

  taxdumprObject <- Taxdumpr(nodesDmpLocation = nodesDmpLocation, namesDmpLocation = namesDmpLocation)

  #' ## Load the Centrifuge results
  #'
  rawCentrifugeProfileDf <- loadRawCentrifugeProfile(rawCentrifugeProfileLocation)
  rawCentrifugeClassificationsDf <- loadRawCentrifugeClassifications(rawCentrifugeClassificationsLocation)
  centrifugeProfileForShotgunDf <- makeCentrifugeProfileForShotgun(rawCentrifugeProfileDf, rawCentrifugeClassificationsDf, taxdumprObject)


  #' ## Load the Spingo results
  #'
  spingoProfileDf <- makeSpingoProfileForAmplicon(rawSpingoProfileLocation,
                                                  rawSpingoProfileClassifications,
                                                  taxdumprObject)


  #' ## Load the QIIME results
  #'
  qiimeProfileDf <- makeQiimeProfileForAmplicon(rawQiimeProfileLocation,
                                                rawQiimeProfileClassifications,
                                                taxdumprObject)


  #' ## Merge profiles
  integratedProfileDf <- mergeProfiles(centrifugeGungaProfileForSh = centrifugeProfileForShotgunDf,
                                       spingoGungaProfileDf = spingoProfileDf,
                                       qiimeGungaProfileDf = qiimeProfileDf,
                                       taxdumprObject = taxdumprObject)


  #' ## Classify True and False Positives
  integratedProfileDf <- clasifyCentrifugeShotgunSpingoAndQiimeGungaProfile(integratedProfileDf)

  return(integratedProfileDf)
}
