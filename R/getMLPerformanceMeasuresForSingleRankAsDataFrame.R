getMLPerformanceMeasureForSingleRankAsDataFrame <- function (expectedIds, predictedIds, allIds)
{
  expectedIds <- unique(expectedIds)
  predictedIds <- unique(predictedIds)
  allIds <- unique(allIds)

  currentTP <- sum(predictedIds %in% expectedIds)
  currentFP <- sum(predictedIds %in% expectedIds == F)
  currentTN <- sum(allIds %in% predictedIds == F & allIds %in% expectedIds == F)
  currentExpectedTP <- length(expectedIds)
  currentFN <- abs(currentExpectedTP - currentTP)
  currentPrecision <- currentTP/(currentTP + currentFP)
  currentRecall <- currentTP/(currentTP + currentFN)
  currentF1 <- (2 * currentPrecision * currentRecall)/(currentPrecision + currentRecall)
  currentAccuracy <- (currentTP + currentTN)/(currentTP + currentFP + currentFN + currentTN)
  currentSpecificity <- currentTN/(currentTN + currentFP)

  performanceDf <- data.frame(expectedTP = currentExpectedTP,
                              TP = currentTP,
                              FP = currentFP,
                              FN = currentFN,
                              TN = currentTN,
                              Precision = currentPrecision,
                              Recall = currentRecall,
                              F1 = currentF1,
                              Accuracy = currentAccuracy,
                              Specificity = currentSpecificity)
  performanceDf
}
