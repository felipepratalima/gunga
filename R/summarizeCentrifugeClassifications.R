require(magrittr)
require(dplyr)

.getDirectCentrifugeClassificationsSummaries <- function(centrifugeClassifications = NULL) {
  directClassificationsSummaries <- centrifugeClassifications %>%
    subset(standardId %>% is.na == F) %>%
    group_by(rank, standardId) %>%
    summarize(directNumberOfSequences = n(),
              directScoreMean = mean(score),
              directScoreSD = sd(score),
              directScoreCI = qnorm(0.975) * sd(score) / sqrt(n()),
              directScoreMedian = median(score),
              directScoreQ1 = quantile(score, 0.25),
              directScoreQ3 = quantile(score, 0.75),
              directScoreIQR = IQR(score),
              directScoreMAD = mad(score),
              directScoreNonNormalMAD = mad(score, constant = 1 / quantile(score, 0.75)),
              directX2ndBestScoreMean = mean(X2ndBestScore),
              directX2ndBestScoreSD = sd(X2ndBestScore),
              directX2ndBestScoreCI = qnorm(0.975) * sd(X2ndBestScore) / sqrt(n()),
              directX2ndBestScoreMedian = median(X2ndBestScore),
              directX2ndBestScoreQ1 = quantile(X2ndBestScore, 0.25),
              directX2ndBestScoreQ3 = quantile(X2ndBestScore, 0.75),
              directX2ndBestScoreIQR = IQR(X2ndBestScore),
              directX2ndBestScoreMAD = mad(X2ndBestScore),
              directX2ndBestScoreNonNormalMAD = mad(X2ndBestScore, constant = 1 / quantile(X2ndBestScore, 0.75)),
              directHitLengthMean = mean(hitLength),
              directHitLengthSD = sd(hitLength),
              directHitLengthCI = qnorm(0.975) * sd(hitLength) / sqrt(n()),
              directHitLengthMedian = median(hitLength),
              directHitLengthQ1 = quantile(hitLength, 0.25),
              directHitLengthQ3 = quantile(hitLength, 0.75),
              directHitLengthIQR = IQR(hitLength),
              directHitLengthMAD = mad(hitLength),
              directHitLengthNonNormalMAD = mad(hitLength, constant = 1 / quantile(hitLength, 0.75)),
              directQueryLengthMean = mean(queryLength),
              directQueryLengthSD = sd(queryLength),
              directQueryLengthCI = qnorm(0.975) * sd(queryLength) / sqrt(n()),
              directQueryLengthMedian = median(queryLength),
              directQueryLengthQ1 = quantile(queryLength, 0.25),
              directQueryLengthQ3 = quantile(queryLength, 0.75),
              directQueryLengthIQR = IQR(queryLength),
              directQueryLengthMAD = mad(queryLength),
              directQueryLengthNonNormalMAD = mad(queryLength, constant = 1 / quantile(queryLength, 0.75)),
              directNumMatchesMean = mean(numMatches),
              directNumMatchesSD = sd(numMatches),
              directNumMatchesCI = qnorm(0.975) * sd(numMatches) / sqrt(n()),
              directNumMatchesMedian = median(numMatches),
              directNumMatchesQ1 = quantile(numMatches, 0.25),
              directNumMatchesQ3 = quantile(numMatches, 0.75),
              directNumMatchesIQR = IQR(numMatches),
              directNumMatchesMAD = mad(numMatches),
              directNumMatchesNonNormalMAD = mad(numMatches, constant = 1 / quantile(numMatches, 0.75))
    )
  return(directClassificationsSummaries)
}

.getSpeciesMappedCentrifugeClassificationsSummaries <- function(centrifugeClassifications = NULL) {
  speciesClassificationsSummaries <- centrifugeClassifications %>%
    subset(speciesId %>% is.na == F) %>%
    group_by(speciesId) %>%
    summarize(mappedNumberOfSequences = n(),
              mappedScoreMean = mean(score),
              mappedScoreSD = sd(score),
              mappedScoreCI = qnorm(0.975) * sd(score) / sqrt(n()),
              mappedScoreMedian = median(score),
              mappedScoreQ1 = quantile(score, 0.25),
              mappedScoreQ3 = quantile(score, 0.75),
              mappedScoreIQR = IQR(score),
              mappedScoreMAD = mad(score),
              mappedScoreNonNormalMAD = mad(score, constant = 1 / quantile(score, 0.75)),
              mappedX2ndBestScoreMean = mean(X2ndBestScore),
              mappedX2ndBestScoreSD = sd(X2ndBestScore),
              mappedX2ndBestScoreCI = qnorm(0.975) * sd(X2ndBestScore) / sqrt(n()),
              mappedX2ndBestScoreMedian = median(X2ndBestScore),
              mappedX2ndBestScoreQ1 = quantile(X2ndBestScore, 0.25),
              mappedX2ndBestScoreQ3 = quantile(X2ndBestScore, 0.75),
              mappedX2ndBestScoreIQR = IQR(X2ndBestScore),
              mappedX2ndBestScoreMAD = mad(X2ndBestScore),
              mappedX2ndBestScoreNonNormalMAD = mad(X2ndBestScore, constant = 1 / quantile(X2ndBestScore, 0.75)),
              mappedHitLengthMean = mean(hitLength),
              mappedHitLengthSD = sd(hitLength),
              mappedHitLengthCI = qnorm(0.975) * sd(hitLength) / sqrt(n()),
              mappedHitLengthMedian = median(hitLength),
              mappedHitLengthQ1 = quantile(hitLength, 0.25),
              mappedHitLengthQ3 = quantile(hitLength, 0.75),
              mappedHitLengthIQR = IQR(hitLength),
              mappedHitLengthMAD = mad(hitLength),
              mappedHitLengthNonNormalMAD = mad(hitLength, constant = 1 / quantile(hitLength, 0.75)),
              mappedQueryLengthMean = mean(queryLength),
              mappedQueryLengthSD = sd(queryLength),
              mappedQueryLengthCI = qnorm(0.975) * sd(queryLength) / sqrt(n()),
              mappedQueryLengthMedian = median(queryLength),
              mappedQueryLengthQ1 = quantile(queryLength, 0.25),
              mappedQueryLengthQ3 = quantile(queryLength, 0.75),
              mappedQueryLengthIQR = IQR(queryLength),
              mappedQueryLengthMAD = mad(queryLength),
              mappedQueryLengthNonNormalMAD = mad(queryLength, constant = 1 / quantile(queryLength, 0.75)),
              mappedNumMatchesMean = mean(numMatches),
              mappedNumMatchesSD = sd(numMatches),
              mappedNumMatchesCI = qnorm(0.975) * sd(numMatches) / sqrt(n()),
              mappedNumMatchesMedian = median(numMatches),
              mappedNumMatchesQ1 = quantile(numMatches, 0.25),
              mappedNumMatchesQ3 = quantile(numMatches, 0.75),
              mappedNumMatchesIQR = IQR(numMatches),
              mappedNumMatchesMAD = mad(numMatches),
              mappedNumMatchesNonNormalMAD = mad(numMatches, constant = 1 / quantile(numMatches, 0.75))
    ) %>%
    rename(standardId = speciesId) %>%
    mutate(rank = "species")
  return(speciesClassificationsSummaries)
}

.getGenusMappedCentrifugeClassificationsSummaries <- function(centrifugeClassifications = NULL) {
  genusClassificationsSummaries <- centrifugeClassifications %>%
    subset(genusId %>% is.na == F) %>%
    group_by(genusId) %>%
    summarize(mappedNumberOfSequences = n(),
              mappedScoreMean = mean(score),
              mappedScoreSD = sd(score),
              mappedScoreCI = qnorm(0.975) * sd(score) / sqrt(n()),
              mappedScoreMedian = median(score),
              mappedScoreQ1 = quantile(score, 0.25),
              mappedScoreQ3 = quantile(score, 0.75),
              mappedScoreIQR = IQR(score),
              mappedScoreMAD = mad(score),
              mappedScoreNonNormalMAD = mad(score, constant = 1 / quantile(score, 0.75)),
              mappedX2ndBestScoreMean = mean(X2ndBestScore),
              mappedX2ndBestScoreSD = sd(X2ndBestScore),
              mappedX2ndBestScoreCI = qnorm(0.975) * sd(X2ndBestScore) / sqrt(n()),
              mappedX2ndBestScoreMedian = median(X2ndBestScore),
              mappedX2ndBestScoreQ1 = quantile(X2ndBestScore, 0.25),
              mappedX2ndBestScoreQ3 = quantile(X2ndBestScore, 0.75),
              mappedX2ndBestScoreIQR = IQR(X2ndBestScore),
              mappedX2ndBestScoreMAD = mad(X2ndBestScore),
              mappedX2ndBestScoreNonNormalMAD = mad(X2ndBestScore, constant = 1 / quantile(X2ndBestScore, 0.75)),
              mappedHitLengthMean = mean(hitLength),
              mappedHitLengthSD = sd(hitLength),
              mappedHitLengthCI = qnorm(0.975) * sd(hitLength) / sqrt(n()),
              mappedHitLengthMedian = median(hitLength),
              mappedHitLengthQ1 = quantile(hitLength, 0.25),
              mappedHitLengthQ3 = quantile(hitLength, 0.75),
              mappedHitLengthIQR = IQR(hitLength),
              mappedHitLengthMAD = mad(hitLength),
              mappedHitLengthNonNormalMAD = mad(hitLength, constant = 1 / quantile(hitLength, 0.75)),
              mappedQueryLengthMean = mean(queryLength),
              mappedQueryLengthSD = sd(queryLength),
              mappedQueryLengthCI = qnorm(0.975) * sd(queryLength) / sqrt(n()),
              mappedQueryLengthMedian = median(queryLength),
              mappedQueryLengthQ1 = quantile(queryLength, 0.25),
              mappedQueryLengthQ3 = quantile(queryLength, 0.75),
              mappedQueryLengthIQR = IQR(queryLength),
              mappedQueryLengthMAD = mad(queryLength),
              mappedQueryLengthNonNormalMAD = mad(queryLength, constant = 1 / quantile(queryLength, 0.75)),
              mappedNumMatchesMean = mean(numMatches),
              mappedNumMatchesSD = sd(numMatches),
              mappedNumMatchesCI = qnorm(0.975) * sd(numMatches) / sqrt(n()),
              mappedNumMatchesMedian = median(numMatches),
              mappedNumMatchesQ1 = quantile(numMatches, 0.25),
              mappedNumMatchesQ3 = quantile(numMatches, 0.75),
              mappedNumMatchesIQR = IQR(numMatches),
              mappedNumMatchesMAD = mad(numMatches),
              mappedNumMatchesNonNormalMAD = mad(numMatches, constant = 1 / quantile(numMatches, 0.75))
    ) %>%
    rename(standardId = genusId) %>%
    mutate(rank = "genus")
  return(genusClassificationsSummaries)
}

.getFamilyMappedCentrifugeClassificationsSummaries <- function(centrifugeClassifications = NULL) {
  familyClassificationsSummaries <- centrifugeClassifications %>%
    subset(familyId %>% is.na == F) %>%
    group_by(familyId) %>%
    summarize(mappedNumberOfSequences = n(),
              mappedScoreMean = mean(score),
              mappedScoreSD = sd(score),
              mappedScoreCI = qnorm(0.975) * sd(score) / sqrt(n()),
              mappedScoreMedian = median(score),
              mappedScoreQ1 = quantile(score, 0.25),
              mappedScoreQ3 = quantile(score, 0.75),
              mappedScoreIQR = IQR(score),
              mappedScoreMAD = mad(score),
              mappedScoreNonNormalMAD = mad(score, constant = 1 / quantile(score, 0.75)),
              mappedX2ndBestScoreMean = mean(X2ndBestScore),
              mappedX2ndBestScoreSD = sd(X2ndBestScore),
              mappedX2ndBestScoreCI = qnorm(0.975) * sd(X2ndBestScore) / sqrt(n()),
              mappedX2ndBestScoreMedian = median(X2ndBestScore),
              mappedX2ndBestScoreQ1 = quantile(X2ndBestScore, 0.25),
              mappedX2ndBestScoreQ3 = quantile(X2ndBestScore, 0.75),
              mappedX2ndBestScoreIQR = IQR(X2ndBestScore),
              mappedX2ndBestScoreMAD = mad(X2ndBestScore),
              mappedX2ndBestScoreNonNormalMAD = mad(X2ndBestScore, constant = 1 / quantile(X2ndBestScore, 0.75)),
              mappedHitLengthMean = mean(hitLength),
              mappedHitLengthSD = sd(hitLength),
              mappedHitLengthCI = qnorm(0.975) * sd(hitLength) / sqrt(n()),
              mappedHitLengthMedian = median(hitLength),
              mappedHitLengthQ1 = quantile(hitLength, 0.25),
              mappedHitLengthQ3 = quantile(hitLength, 0.75),
              mappedHitLengthIQR = IQR(hitLength),
              mappedHitLengthMAD = mad(hitLength),
              mappedHitLengthNonNormalMAD = mad(hitLength, constant = 1 / quantile(hitLength, 0.75)),
              mappedQueryLengthMean = mean(queryLength),
              mappedQueryLengthSD = sd(queryLength),
              mappedQueryLengthCI = qnorm(0.975) * sd(queryLength) / sqrt(n()),
              mappedQueryLengthMedian = median(queryLength),
              mappedQueryLengthQ1 = quantile(queryLength, 0.25),
              mappedQueryLengthQ3 = quantile(queryLength, 0.75),
              mappedQueryLengthIQR = IQR(queryLength),
              mappedQueryLengthMAD = mad(queryLength),
              mappedQueryLengthNonNormalMAD = mad(queryLength, constant = 1 / quantile(queryLength, 0.75)),
              mappedNumMatchesMean = mean(numMatches),
              mappedNumMatchesSD = sd(numMatches),
              mappedNumMatchesCI = qnorm(0.975) * sd(numMatches) / sqrt(n()),
              mappedNumMatchesMedian = median(numMatches),
              mappedNumMatchesQ1 = quantile(numMatches, 0.25),
              mappedNumMatchesQ3 = quantile(numMatches, 0.75),
              mappedNumMatchesIQR = IQR(numMatches),
              mappedNumMatchesMAD = mad(numMatches),
              mappedNumMatchesNonNormalMAD = mad(numMatches, constant = 1 / quantile(numMatches, 0.75))
    ) %>%
    rename(standardId = familyId) %>%
    mutate(rank = "family")
  return(familyClassificationsSummaries)
}

.getOrderMappedCentrifugeClassificationsSummaries <- function(centrifugeClassifications = NULL) {
  orderClassificationsSummaries <- centrifugeClassifications %>%
    subset(orderId %>% is.na == F) %>%
    group_by(orderId) %>%
    summarize(mappedNumberOfSequences = n(),
              mappedScoreMean = mean(score),
              mappedScoreSD = sd(score),
              mappedScoreCI = qnorm(0.975) * sd(score) / sqrt(n()),
              mappedScoreMedian = median(score),
              mappedScoreQ1 = quantile(score, 0.25),
              mappedScoreQ3 = quantile(score, 0.75),
              mappedScoreIQR = IQR(score),
              mappedScoreMAD = mad(score),
              mappedScoreNonNormalMAD = mad(score, constant = 1 / quantile(score, 0.75)),
              mappedX2ndBestScoreMean = mean(X2ndBestScore),
              mappedX2ndBestScoreSD = sd(X2ndBestScore),
              mappedX2ndBestScoreCI = qnorm(0.975) * sd(X2ndBestScore) / sqrt(n()),
              mappedX2ndBestScoreMedian = median(X2ndBestScore),
              mappedX2ndBestScoreQ1 = quantile(X2ndBestScore, 0.25),
              mappedX2ndBestScoreQ3 = quantile(X2ndBestScore, 0.75),
              mappedX2ndBestScoreIQR = IQR(X2ndBestScore),
              mappedX2ndBestScoreMAD = mad(X2ndBestScore),
              mappedX2ndBestScoreNonNormalMAD = mad(X2ndBestScore, constant = 1 / quantile(X2ndBestScore, 0.75)),
              mappedHitLengthMean = mean(hitLength),
              mappedHitLengthSD = sd(hitLength),
              mappedHitLengthCI = qnorm(0.975) * sd(hitLength) / sqrt(n()),
              mappedHitLengthMedian = median(hitLength),
              mappedHitLengthQ1 = quantile(hitLength, 0.25),
              mappedHitLengthQ3 = quantile(hitLength, 0.75),
              mappedHitLengthIQR = IQR(hitLength),
              mappedHitLengthMAD = mad(hitLength),
              mappedHitLengthNonNormalMAD = mad(hitLength, constant = 1 / quantile(hitLength, 0.75)),
              mappedQueryLengthMean = mean(queryLength),
              mappedQueryLengthSD = sd(queryLength),
              mappedQueryLengthCI = qnorm(0.975) * sd(queryLength) / sqrt(n()),
              mappedQueryLengthMedian = median(queryLength),
              mappedQueryLengthQ1 = quantile(queryLength, 0.25),
              mappedQueryLengthQ3 = quantile(queryLength, 0.75),
              mappedQueryLengthIQR = IQR(queryLength),
              mappedQueryLengthMAD = mad(queryLength),
              mappedQueryLengthNonNormalMAD = mad(queryLength, constant = 1 / quantile(queryLength, 0.75)),
              mappedNumMatchesMean = mean(numMatches),
              mappedNumMatchesSD = sd(numMatches),
              mappedNumMatchesCI = qnorm(0.975) * sd(numMatches) / sqrt(n()),
              mappedNumMatchesMedian = median(numMatches),
              mappedNumMatchesQ1 = quantile(numMatches, 0.25),
              mappedNumMatchesQ3 = quantile(numMatches, 0.75),
              mappedNumMatchesIQR = IQR(numMatches),
              mappedNumMatchesMAD = mad(numMatches),
              mappedNumMatchesNonNormalMAD = mad(numMatches, constant = 1 / quantile(numMatches, 0.75))
    ) %>%
    rename(standardId = orderId) %>%
    mutate(rank = "order")
  return(orderClassificationsSummaries)
}

.getClassMappedCentrifugeClassificationsSummaries <- function(centrifugeClassifications = NULL) {
  classClassificationsSummaries <- centrifugeClassifications %>%
    subset(classId %>% is.na == F) %>%
    group_by(classId) %>%
    summarize(mappedNumberOfSequences = n(),
              mappedScoreMean = mean(score),
              mappedScoreSD = sd(score),
              mappedScoreCI = qnorm(0.975) * sd(score) / sqrt(n()),
              mappedScoreMedian = median(score),
              mappedScoreQ1 = quantile(score, 0.25),
              mappedScoreQ3 = quantile(score, 0.75),
              mappedScoreIQR = IQR(score),
              mappedScoreMAD = mad(score),
              mappedScoreNonNormalMAD = mad(score, constant = 1 / quantile(score, 0.75)),
              mappedX2ndBestScoreMean = mean(X2ndBestScore),
              mappedX2ndBestScoreSD = sd(X2ndBestScore),
              mappedX2ndBestScoreCI = qnorm(0.975) * sd(X2ndBestScore) / sqrt(n()),
              mappedX2ndBestScoreMedian = median(X2ndBestScore),
              mappedX2ndBestScoreQ1 = quantile(X2ndBestScore, 0.25),
              mappedX2ndBestScoreQ3 = quantile(X2ndBestScore, 0.75),
              mappedX2ndBestScoreIQR = IQR(X2ndBestScore),
              mappedX2ndBestScoreMAD = mad(X2ndBestScore),
              mappedX2ndBestScoreNonNormalMAD = mad(X2ndBestScore, constant = 1 / quantile(X2ndBestScore, 0.75)),
              mappedHitLengthMean = mean(hitLength),
              mappedHitLengthSD = sd(hitLength),
              mappedHitLengthCI = qnorm(0.975) * sd(hitLength) / sqrt(n()),
              mappedHitLengthMedian = median(hitLength),
              mappedHitLengthQ1 = quantile(hitLength, 0.25),
              mappedHitLengthQ3 = quantile(hitLength, 0.75),
              mappedHitLengthIQR = IQR(hitLength),
              mappedHitLengthMAD = mad(hitLength),
              mappedHitLengthNonNormalMAD = mad(hitLength, constant = 1 / quantile(hitLength, 0.75)),
              mappedQueryLengthMean = mean(queryLength),
              mappedQueryLengthSD = sd(queryLength),
              mappedQueryLengthCI = qnorm(0.975) * sd(queryLength) / sqrt(n()),
              mappedQueryLengthMedian = median(queryLength),
              mappedQueryLengthQ1 = quantile(queryLength, 0.25),
              mappedQueryLengthQ3 = quantile(queryLength, 0.75),
              mappedQueryLengthIQR = IQR(queryLength),
              mappedQueryLengthMAD = mad(queryLength),
              mappedQueryLengthNonNormalMAD = mad(queryLength, constant = 1 / quantile(queryLength, 0.75)),
              mappedNumMatchesMean = mean(numMatches),
              mappedNumMatchesSD = sd(numMatches),
              mappedNumMatchesCI = qnorm(0.975) * sd(numMatches) / sqrt(n()),
              mappedNumMatchesMedian = median(numMatches),
              mappedNumMatchesQ1 = quantile(numMatches, 0.25),
              mappedNumMatchesQ3 = quantile(numMatches, 0.75),
              mappedNumMatchesIQR = IQR(numMatches),
              mappedNumMatchesMAD = mad(numMatches),
              mappedNumMatchesNonNormalMAD = mad(numMatches, constant = 1 / quantile(numMatches, 0.75))
    ) %>%
    rename(standardId = classId) %>%
    mutate(rank = "class")
  return(classClassificationsSummaries)
}

.getPhylumMappedCentrifugeClassificationsSummaries <- function(centrifugeClassifications = NULL) {
  phylumClassificationsSummaries <- centrifugeClassifications %>%
    subset(phylumId %>% is.na == F) %>%
    group_by(phylumId) %>%
    summarize(mappedNumberOfSequences = n(),
              mappedScoreMean = mean(score),
              mappedScoreSD = sd(score),
              mappedScoreCI = qnorm(0.975) * sd(score) / sqrt(n()),
              mappedScoreMedian = median(score),
              mappedScoreQ1 = quantile(score, 0.25),
              mappedScoreQ3 = quantile(score, 0.75),
              mappedScoreIQR = IQR(score),
              mappedScoreMAD = mad(score),
              mappedScoreNonNormalMAD = mad(score, constant = 1 / quantile(score, 0.75)),
              mappedX2ndBestScoreMean = mean(X2ndBestScore),
              mappedX2ndBestScoreSD = sd(X2ndBestScore),
              mappedX2ndBestScoreCI = qnorm(0.975) * sd(X2ndBestScore) / sqrt(n()),
              mappedX2ndBestScoreMedian = median(X2ndBestScore),
              mappedX2ndBestScoreQ1 = quantile(X2ndBestScore, 0.25),
              mappedX2ndBestScoreQ3 = quantile(X2ndBestScore, 0.75),
              mappedX2ndBestScoreIQR = IQR(X2ndBestScore),
              mappedX2ndBestScoreMAD = mad(X2ndBestScore),
              mappedX2ndBestScoreNonNormalMAD = mad(X2ndBestScore, constant = 1 / quantile(X2ndBestScore, 0.75)),
              mappedHitLengthMean = mean(hitLength),
              mappedHitLengthSD = sd(hitLength),
              mappedHitLengthCI = qnorm(0.975) * sd(hitLength) / sqrt(n()),
              mappedHitLengthMedian = median(hitLength),
              mappedHitLengthQ1 = quantile(hitLength, 0.25),
              mappedHitLengthQ3 = quantile(hitLength, 0.75),
              mappedHitLengthIQR = IQR(hitLength),
              mappedHitLengthMAD = mad(hitLength),
              mappedHitLengthNonNormalMAD = mad(hitLength, constant = 1 / quantile(hitLength, 0.75)),
              mappedQueryLengthMean = mean(queryLength),
              mappedQueryLengthSD = sd(queryLength),
              mappedQueryLengthCI = qnorm(0.975) * sd(queryLength) / sqrt(n()),
              mappedQueryLengthMedian = median(queryLength),
              mappedQueryLengthQ1 = quantile(queryLength, 0.25),
              mappedQueryLengthQ3 = quantile(queryLength, 0.75),
              mappedQueryLengthIQR = IQR(queryLength),
              mappedQueryLengthMAD = mad(queryLength),
              mappedQueryLengthNonNormalMAD = mad(queryLength, constant = 1 / quantile(queryLength, 0.75)),
              mappedNumMatchesMean = mean(numMatches),
              mappedNumMatchesSD = sd(numMatches),
              mappedNumMatchesCI = qnorm(0.975) * sd(numMatches) / sqrt(n()),
              mappedNumMatchesMedian = median(numMatches),
              mappedNumMatchesQ1 = quantile(numMatches, 0.25),
              mappedNumMatchesQ3 = quantile(numMatches, 0.75),
              mappedNumMatchesIQR = IQR(numMatches),
              mappedNumMatchesMAD = mad(numMatches),
              mappedNumMatchesNonNormalMAD = mad(numMatches, constant = 1 / quantile(numMatches, 0.75))
    ) %>%
    rename(standardId = phylumId) %>%
    mutate(rank = "phylum")
  return(phylumClassificationsSummaries)
}

.getSuperkingdomMappedCentrifugeClassificationsSummaries <- function(centrifugeClassifications = NULL) {
  superkingdomClassificationsSummaries <- centrifugeClassifications %>%
    subset(superkingdomId %>% is.na == F) %>%
    group_by(superkingdomId) %>%
    summarize(mappedNumberOfSequences = n(),
              mappedScoreMean = mean(score),
              mappedScoreSD = sd(score),
              mappedScoreCI = qnorm(0.975) * sd(score) / sqrt(n()),
              mappedScoreMedian = median(score),
              mappedScoreQ1 = quantile(score, 0.25),
              mappedScoreQ3 = quantile(score, 0.75),
              mappedScoreIQR = IQR(score),
              mappedScoreMAD = mad(score),
              mappedScoreNonNormalMAD = mad(score, constant = 1 / quantile(score, 0.75)),
              mappedX2ndBestScoreMean = mean(X2ndBestScore),
              mappedX2ndBestScoreSD = sd(X2ndBestScore),
              mappedX2ndBestScoreCI = qnorm(0.975) * sd(X2ndBestScore) / sqrt(n()),
              mappedX2ndBestScoreMedian = median(X2ndBestScore),
              mappedX2ndBestScoreQ1 = quantile(X2ndBestScore, 0.25),
              mappedX2ndBestScoreQ3 = quantile(X2ndBestScore, 0.75),
              mappedX2ndBestScoreIQR = IQR(X2ndBestScore),
              mappedX2ndBestScoreMAD = mad(X2ndBestScore),
              mappedX2ndBestScoreNonNormalMAD = mad(X2ndBestScore, constant = 1 / quantile(X2ndBestScore, 0.75)),
              mappedHitLengthMean = mean(hitLength),
              mappedHitLengthSD = sd(hitLength),
              mappedHitLengthCI = qnorm(0.975) * sd(hitLength) / sqrt(n()),
              mappedHitLengthMedian = median(hitLength),
              mappedHitLengthQ1 = quantile(hitLength, 0.25),
              mappedHitLengthQ3 = quantile(hitLength, 0.75),
              mappedHitLengthIQR = IQR(hitLength),
              mappedHitLengthMAD = mad(hitLength),
              mappedHitLengthNonNormalMAD = mad(hitLength, constant = 1 / quantile(hitLength, 0.75)),
              mappedQueryLengthMean = mean(queryLength),
              mappedQueryLengthSD = sd(queryLength),
              mappedQueryLengthCI = qnorm(0.975) * sd(queryLength) / sqrt(n()),
              mappedQueryLengthMedian = median(queryLength),
              mappedQueryLengthQ1 = quantile(queryLength, 0.25),
              mappedQueryLengthQ3 = quantile(queryLength, 0.75),
              mappedQueryLengthIQR = IQR(queryLength),
              mappedQueryLengthMAD = mad(queryLength),
              mappedQueryLengthNonNormalMAD = mad(queryLength, constant = 1 / quantile(queryLength, 0.75)),
              mappedNumMatchesMean = mean(numMatches),
              mappedNumMatchesSD = sd(numMatches),
              mappedNumMatchesCI = qnorm(0.975) * sd(numMatches) / sqrt(n()),
              mappedNumMatchesMedian = median(numMatches),
              mappedNumMatchesQ1 = quantile(numMatches, 0.25),
              mappedNumMatchesQ3 = quantile(numMatches, 0.75),
              mappedNumMatchesIQR = IQR(numMatches),
              mappedNumMatchesMAD = mad(numMatches),
              mappedNumMatchesNonNormalMAD = mad(numMatches, constant = 1 / quantile(numMatches, 0.75))
    ) %>%
    rename(standardId = superkingdomId) %>%
    mutate(rank = "superkingdom")
  return(superkingdomClassificationsSummaries)
}

#'
#'
#' @param centrifugeClassifications
summarizeCentrifugeClassifications <- function(centrifugeClassifications = NULL) {
  directClassificationsSummaries <- .getDirectCentrifugeClassificationsSummaries(centrifugeClassifications)

  superkingdomClassificationsSummaries <- .getSuperkingdomMappedCentrifugeClassificationsSummaries(centrifugeClassifications)
  phylumClassificationsSummaries <- .getPhylumMappedCentrifugeClassificationsSummaries(centrifugeClassifications)
  classClassificationsSummaries <- .getClassMappedCentrifugeClassificationsSummaries(centrifugeClassifications)
  orderClassificationsSummaries <- .getOrderMappedCentrifugeClassificationsSummaries(centrifugeClassifications)
  familyClassificationsSummaries <- .getFamilyMappedCentrifugeClassificationsSummaries(centrifugeClassifications)
  genusClassificationsSummaries <- .getGenusMappedCentrifugeClassificationsSummaries(centrifugeClassifications)
  speciesClassificationsSummaries <- .getSpeciesMappedCentrifugeClassificationsSummaries(centrifugeClassifications)

  mappedClassificationsSummaries <- rbind(
    superkingdomClassificationsSummaries,
    phylumClassificationsSummaries,
    classClassificationsSummaries,
    orderClassificationsSummaries,
    familyClassificationsSummaries,
    genusClassificationsSummaries,
    speciesClassificationsSummaries
  )

  centrifugeClasificationstSummaries <- directClassificationsSummaries %>%
    merge(mappedClassificationsSummaries, by = c("rank", "standardId"), all = T)

  return(centrifugeClasificationstSummaries)
}
