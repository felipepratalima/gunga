require(magrittr)
require(dplyr)

.getDirectSpingoClassificationsSummaries <- function(spingoClassifications = NULL) {
  directClassificationsSummaries <- spingoClassifications %>%
    subset(standardId %>% is.na == F) %>%
    group_by(rank, standardId, spingoClostridiumGroup) %>%
    summarize(directSpingoNumberOfSequences = n(),
              directSpingoScoreMean = mean(spingoScore),
              directSpingoScoreSD = sd(spingoScore),
              directSpingoScoreCI = qnorm(0.975) * sd(spingoScore) / sqrt(n()),
              directSpingoScoreMedian = median(spingoScore),
              directSpingoScoreQ1 = quantile(spingoScore, 0.25),
              directSpingoScoreQ3 = quantile(spingoScore, 0.75),
              directSpingoScoreIQR = IQR(spingoScore),
              directSpingoScoreMAD = mad(spingoScore),
              directSpingoScoreNonNormalMAD = mad(spingoScore, constant = 1 / quantile(spingoScore, 0.75)),
              directSpingoClostridiumGroupScoreMean = mean(spingoClostridiumGroupScore),
              directSpingoClostridiumGroupScoreSD = sd(spingoClostridiumGroupScore),
              directSpingoClostridiumGroupScoreCI = qnorm(0.975) * sd(spingoClostridiumGroupScore) / sqrt(n()),
              directSpingoClostridiumGroupScoreMedian = median(spingoClostridiumGroupScore),
              directSpingoClostridiumGroupScoreQ1 = quantile(spingoClostridiumGroupScore, 0.25),
              directSpingoClostridiumGroupScoreQ3 = quantile(spingoClostridiumGroupScore, 0.75),
              directSpingoClostridiumGroupScoreIQR = IQR(spingoClostridiumGroupScore),
              directSpingoClostridiumGroupScoreMAD = mad(spingoClostridiumGroupScore),
              directSpingoClostridiumGroupScoreNonNormalMAD = mad(spingoClostridiumGroupScore, constant = 1 / quantile(spingoClostridiumGroupScore, 0.75)),
              directSpingoGenusScoreMean = mean(spingoGenusScore),
              directSpingoGenusScoreSD = sd(spingoGenusScore),
              directSpingoGenusScoreCI = qnorm(0.975) * sd(spingoGenusScore) / sqrt(n()),
              directSpingoGenusScoreMedian = median(spingoGenusScore),
              directSpingoGenusScoreQ1 = quantile(spingoGenusScore, 0.25),
              directSpingoGenusScoreQ3 = quantile(spingoGenusScore, 0.75),
              directSpingoGenusScoreIQR = IQR(spingoGenusScore),
              directSpingoGenusScoreMAD = mad(spingoGenusScore),
              directSpingoGenusScoreNonNormalMAD = mad(spingoGenusScore, constant = 1 / quantile(spingoGenusScore, 0.75)),
              directSpingoSpeciesScoreMean = mean(spingoSpeciesScore),
              directSpingoSpeciesScoreSD = sd(spingoSpeciesScore),
              directSpingoSpeciesScoreCI = qnorm(0.975) * sd(spingoSpeciesScore) / sqrt(n()),
              directSpingoSpeciesScoreMedian = median(spingoSpeciesScore),
              directSpingoSpeciesScoreQ1 = quantile(spingoSpeciesScore, 0.25),
              directSpingoSpeciesScoreQ3 = quantile(spingoSpeciesScore, 0.75),
              directSpingoSpeciesScoreIQR = IQR(spingoSpeciesScore),
              directSpingoSpeciesScoreMAD = mad(spingoSpeciesScore),
              directSpingoSpeciesScoreNonNormalMAD = mad(spingoSpeciesScore, constant = 1 / quantile(spingoSpeciesScore, 0.75))
    )
  return(directClassificationsSummaries)
}

.getSpeciesMappedSpingoClassificationsSummaries <- function(spingoClassifications = NULL) {
  speciesClassificationsSummaries <- spingoClassifications %>%
    subset(speciesId %>% is.na == F) %>%
    group_by(speciesId, spingoClostridiumGroup) %>%
    summarize(mappedSpingoNumberOfSequences = n(),
              mappedSpingoScoreMean = mean(spingoScore),
              mappedSpingoScoreSD = sd(spingoScore),
              mappedSpingoScoreCI = qnorm(0.975) * sd(spingoScore) / sqrt(n()),
              mappedSpingoScoreMedian = median(spingoScore),
              mappedSpingoScoreQ1 = quantile(spingoScore, 0.25),
              mappedSpingoScoreQ3 = quantile(spingoScore, 0.75),
              mappedSpingoScoreIQR = IQR(spingoScore),
              mappedSpingoScoreMAD = mad(spingoScore),
              mappedSpingoScoreNonNormalMAD = mad(spingoScore, constant = 1 / quantile(spingoScore, 0.75)),
              mappedSpingoClostridiumGroupScoreMean = mean(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreSD = sd(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreCI = qnorm(0.975) * sd(spingoClostridiumGroupScore) / sqrt(n()),
              mappedSpingoClostridiumGroupScoreMedian = median(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreQ1 = quantile(spingoClostridiumGroupScore, 0.25),
              mappedSpingoClostridiumGroupScoreQ3 = quantile(spingoClostridiumGroupScore, 0.75),
              mappedSpingoClostridiumGroupScoreIQR = IQR(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreMAD = mad(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreNonNormalMAD = mad(spingoClostridiumGroupScore, constant = 1 / quantile(spingoClostridiumGroupScore, 0.75)),
              mappedSpingoGenusScoreMean = mean(spingoGenusScore),
              mappedSpingoGenusScoreSD = sd(spingoGenusScore),
              mappedSpingoGenusScoreCI = qnorm(0.975) * sd(spingoGenusScore) / sqrt(n()),
              mappedSpingoGenusScoreMedian = median(spingoGenusScore),
              mappedSpingoGenusScoreQ1 = quantile(spingoGenusScore, 0.25),
              mappedSpingoGenusScoreQ3 = quantile(spingoGenusScore, 0.75),
              mappedSpingoGenusScoreIQR = IQR(spingoGenusScore),
              mappedSpingoGenusScoreMAD = mad(spingoGenusScore),
              mappedSpingoGenusScoreNonNormalMAD = mad(spingoGenusScore, constant = 1 / quantile(spingoGenusScore, 0.75)),
              mappedSpingoSpeciesScoreMean = mean(spingoSpeciesScore),
              mappedSpingoSpeciesScoreSD = sd(spingoSpeciesScore),
              mappedSpingoSpeciesScoreCI = qnorm(0.975) * sd(spingoSpeciesScore) / sqrt(n()),
              mappedSpingoSpeciesScoreMedian = median(spingoSpeciesScore),
              mappedSpingoSpeciesScoreQ1 = quantile(spingoSpeciesScore, 0.25),
              mappedSpingoSpeciesScoreQ3 = quantile(spingoSpeciesScore, 0.75),
              mappedSpingoSpeciesScoreIQR = IQR(spingoSpeciesScore),
              mappedSpingoSpeciesScoreMAD = mad(spingoSpeciesScore),
              mappedSpingoSpeciesScoreNonNormalMAD = mad(spingoSpeciesScore, constant = 1 / quantile(spingoSpeciesScore, 0.75))
    ) %>%
    rename(standardId = speciesId) %>%
    mutate(rank = "species")
  return(speciesClassificationsSummaries)
}

.getGenusMappedSpingoClassificationsSummaries <- function(spingoClassifications = NULL) {
  genusClassificationsSummaries <- spingoClassifications %>%
    subset(genusId %>% is.na == F) %>%
    group_by(genusId, spingoClostridiumGroup) %>%
    summarize(mappedSpingoNumberOfSequences = n(),
              mappedSpingoScoreMean = mean(spingoScore),
              mappedSpingoScoreSD = sd(spingoScore),
              mappedSpingoScoreCI = qnorm(0.975) * sd(spingoScore) / sqrt(n()),
              mappedSpingoScoreMedian = median(spingoScore),
              mappedSpingoScoreQ1 = quantile(spingoScore, 0.25),
              mappedSpingoScoreQ3 = quantile(spingoScore, 0.75),
              mappedSpingoScoreIQR = IQR(spingoScore),
              mappedSpingoScoreMAD = mad(spingoScore),
              mappedSpingoScoreNonNormalMAD = mad(spingoScore, constant = 1 / quantile(spingoScore, 0.75)),
              mappedSpingoClostridiumGroupScoreMean = mean(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreSD = sd(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreCI = qnorm(0.975) * sd(spingoClostridiumGroupScore) / sqrt(n()),
              mappedSpingoClostridiumGroupScoreMedian = median(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreQ1 = quantile(spingoClostridiumGroupScore, 0.25),
              mappedSpingoClostridiumGroupScoreQ3 = quantile(spingoClostridiumGroupScore, 0.75),
              mappedSpingoClostridiumGroupScoreIQR = IQR(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreMAD = mad(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreNonNormalMAD = mad(spingoClostridiumGroupScore, constant = 1 / quantile(spingoClostridiumGroupScore, 0.75)),
              mappedSpingoGenusScoreMean = mean(spingoGenusScore),
              mappedSpingoGenusScoreSD = sd(spingoGenusScore),
              mappedSpingoGenusScoreCI = qnorm(0.975) * sd(spingoGenusScore) / sqrt(n()),
              mappedSpingoGenusScoreMedian = median(spingoGenusScore),
              mappedSpingoGenusScoreQ1 = quantile(spingoGenusScore, 0.25),
              mappedSpingoGenusScoreQ3 = quantile(spingoGenusScore, 0.75),
              mappedSpingoGenusScoreIQR = IQR(spingoGenusScore),
              mappedSpingoGenusScoreMAD = mad(spingoGenusScore),
              mappedSpingoGenusScoreNonNormalMAD = mad(spingoGenusScore, constant = 1 / quantile(spingoGenusScore, 0.75)),
              mappedSpingoSpeciesScoreMean = mean(spingoSpeciesScore),
              mappedSpingoSpeciesScoreSD = sd(spingoSpeciesScore),
              mappedSpingoSpeciesScoreCI = qnorm(0.975) * sd(spingoSpeciesScore) / sqrt(n()),
              mappedSpingoSpeciesScoreMedian = median(spingoSpeciesScore),
              mappedSpingoSpeciesScoreQ1 = quantile(spingoSpeciesScore, 0.25),
              mappedSpingoSpeciesScoreQ3 = quantile(spingoSpeciesScore, 0.75),
              mappedSpingoSpeciesScoreIQR = IQR(spingoSpeciesScore),
              mappedSpingoSpeciesScoreMAD = mad(spingoSpeciesScore),
              mappedSpingoSpeciesScoreNonNormalMAD = mad(spingoSpeciesScore, constant = 1 / quantile(spingoSpeciesScore, 0.75))
    ) %>%
    rename(standardId = genusId) %>%
    mutate(rank = "genus")
  return(genusClassificationsSummaries)
}

.getFamilyMappedSpingoClassificationsSummaries <- function(spingoClassifications = NULL) {
  familyClassificationsSummaries <- spingoClassifications %>%
    subset(familyId %>% is.na == F) %>%
    group_by(familyId, spingoClostridiumGroup) %>%
    summarize(mappedSpingoNumberOfSequences = n(),
              mappedSpingoScoreMean = mean(spingoScore),
              mappedSpingoScoreSD = sd(spingoScore),
              mappedSpingoScoreCI = qnorm(0.975) * sd(spingoScore) / sqrt(n()),
              mappedSpingoScoreMedian = median(spingoScore),
              mappedSpingoScoreQ1 = quantile(spingoScore, 0.25),
              mappedSpingoScoreQ3 = quantile(spingoScore, 0.75),
              mappedSpingoScoreIQR = IQR(spingoScore),
              mappedSpingoScoreMAD = mad(spingoScore),
              mappedSpingoScoreNonNormalMAD = mad(spingoScore, constant = 1 / quantile(spingoScore, 0.75)),
              mappedSpingoClostridiumGroupScoreMean = mean(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreSD = sd(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreCI = qnorm(0.975) * sd(spingoClostridiumGroupScore) / sqrt(n()),
              mappedSpingoClostridiumGroupScoreMedian = median(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreQ1 = quantile(spingoClostridiumGroupScore, 0.25),
              mappedSpingoClostridiumGroupScoreQ3 = quantile(spingoClostridiumGroupScore, 0.75),
              mappedSpingoClostridiumGroupScoreIQR = IQR(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreMAD = mad(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreNonNormalMAD = mad(spingoClostridiumGroupScore, constant = 1 / quantile(spingoClostridiumGroupScore, 0.75)),
              mappedSpingoGenusScoreMean = mean(spingoGenusScore),
              mappedSpingoGenusScoreSD = sd(spingoGenusScore),
              mappedSpingoGenusScoreCI = qnorm(0.975) * sd(spingoGenusScore) / sqrt(n()),
              mappedSpingoGenusScoreMedian = median(spingoGenusScore),
              mappedSpingoGenusScoreQ1 = quantile(spingoGenusScore, 0.25),
              mappedSpingoGenusScoreQ3 = quantile(spingoGenusScore, 0.75),
              mappedSpingoGenusScoreIQR = IQR(spingoGenusScore),
              mappedSpingoGenusScoreMAD = mad(spingoGenusScore),
              mappedSpingoGenusScoreNonNormalMAD = mad(spingoGenusScore, constant = 1 / quantile(spingoGenusScore, 0.75)),
              mappedSpingoSpeciesScoreMean = mean(spingoSpeciesScore),
              mappedSpingoSpeciesScoreSD = sd(spingoSpeciesScore),
              mappedSpingoSpeciesScoreCI = qnorm(0.975) * sd(spingoSpeciesScore) / sqrt(n()),
              mappedSpingoSpeciesScoreMedian = median(spingoSpeciesScore),
              mappedSpingoSpeciesScoreQ1 = quantile(spingoSpeciesScore, 0.25),
              mappedSpingoSpeciesScoreQ3 = quantile(spingoSpeciesScore, 0.75),
              mappedSpingoSpeciesScoreIQR = IQR(spingoSpeciesScore),
              mappedSpingoSpeciesScoreMAD = mad(spingoSpeciesScore),
              mappedSpingoSpeciesScoreNonNormalMAD = mad(spingoSpeciesScore, constant = 1 / quantile(spingoSpeciesScore, 0.75))
    ) %>%
    rename(standardId = familyId) %>%
    mutate(rank = "family")
  return(familyClassificationsSummaries)
}

.getOrderMappedSpingoClassificationsSummaries <- function(spingoClassifications = NULL) {
  orderClassificationsSummaries <- spingoClassifications %>%
    subset(orderId %>% is.na == F) %>%
    group_by(orderId, spingoClostridiumGroup) %>%
    summarize(mappedSpingoNumberOfSequences = n(),
              mappedSpingoScoreMean = mean(spingoScore),
              mappedSpingoScoreSD = sd(spingoScore),
              mappedSpingoScoreCI = qnorm(0.975) * sd(spingoScore) / sqrt(n()),
              mappedSpingoScoreMedian = median(spingoScore),
              mappedSpingoScoreQ1 = quantile(spingoScore, 0.25),
              mappedSpingoScoreQ3 = quantile(spingoScore, 0.75),
              mappedSpingoScoreIQR = IQR(spingoScore),
              mappedSpingoScoreMAD = mad(spingoScore),
              mappedSpingoScoreNonNormalMAD = mad(spingoScore, constant = 1 / quantile(spingoScore, 0.75)),
              mappedSpingoClostridiumGroupScoreMean = mean(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreSD = sd(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreCI = qnorm(0.975) * sd(spingoClostridiumGroupScore) / sqrt(n()),
              mappedSpingoClostridiumGroupScoreMedian = median(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreQ1 = quantile(spingoClostridiumGroupScore, 0.25),
              mappedSpingoClostridiumGroupScoreQ3 = quantile(spingoClostridiumGroupScore, 0.75),
              mappedSpingoClostridiumGroupScoreIQR = IQR(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreMAD = mad(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreNonNormalMAD = mad(spingoClostridiumGroupScore, constant = 1 / quantile(spingoClostridiumGroupScore, 0.75)),
              mappedSpingoGenusScoreMean = mean(spingoGenusScore),
              mappedSpingoGenusScoreSD = sd(spingoGenusScore),
              mappedSpingoGenusScoreCI = qnorm(0.975) * sd(spingoGenusScore) / sqrt(n()),
              mappedSpingoGenusScoreMedian = median(spingoGenusScore),
              mappedSpingoGenusScoreQ1 = quantile(spingoGenusScore, 0.25),
              mappedSpingoGenusScoreQ3 = quantile(spingoGenusScore, 0.75),
              mappedSpingoGenusScoreIQR = IQR(spingoGenusScore),
              mappedSpingoGenusScoreMAD = mad(spingoGenusScore),
              mappedSpingoGenusScoreNonNormalMAD = mad(spingoGenusScore, constant = 1 / quantile(spingoGenusScore, 0.75)),
              mappedSpingoSpeciesScoreMean = mean(spingoSpeciesScore),
              mappedSpingoSpeciesScoreSD = sd(spingoSpeciesScore),
              mappedSpingoSpeciesScoreCI = qnorm(0.975) * sd(spingoSpeciesScore) / sqrt(n()),
              mappedSpingoSpeciesScoreMedian = median(spingoSpeciesScore),
              mappedSpingoSpeciesScoreQ1 = quantile(spingoSpeciesScore, 0.25),
              mappedSpingoSpeciesScoreQ3 = quantile(spingoSpeciesScore, 0.75),
              mappedSpingoSpeciesScoreIQR = IQR(spingoSpeciesScore),
              mappedSpingoSpeciesScoreMAD = mad(spingoSpeciesScore),
              mappedSpingoSpeciesScoreNonNormalMAD = mad(spingoSpeciesScore, constant = 1 / quantile(spingoSpeciesScore, 0.75))
    ) %>%
    rename(standardId = orderId) %>%
    mutate(rank = "order")
  return(orderClassificationsSummaries)
}

.getClassMappedSpingoClassificationsSummaries <- function(spingoClassifications = NULL) {
  classClassificationsSummaries <- spingoClassifications %>%
    subset(classId %>% is.na == F) %>%
    group_by(classId, spingoClostridiumGroup) %>%
    summarize(mappedSpingoNumberOfSequences = n(),
              mappedSpingoScoreMean = mean(spingoScore),
              mappedSpingoScoreSD = sd(spingoScore),
              mappedSpingoScoreCI = qnorm(0.975) * sd(spingoScore) / sqrt(n()),
              mappedSpingoScoreMedian = median(spingoScore),
              mappedSpingoScoreQ1 = quantile(spingoScore, 0.25),
              mappedSpingoScoreQ3 = quantile(spingoScore, 0.75),
              mappedSpingoScoreIQR = IQR(spingoScore),
              mappedSpingoScoreMAD = mad(spingoScore),
              mappedSpingoScoreNonNormalMAD = mad(spingoScore, constant = 1 / quantile(spingoScore, 0.75)),
              mappedSpingoClostridiumGroupScoreMean = mean(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreSD = sd(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreCI = qnorm(0.975) * sd(spingoClostridiumGroupScore) / sqrt(n()),
              mappedSpingoClostridiumGroupScoreMedian = median(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreQ1 = quantile(spingoClostridiumGroupScore, 0.25),
              mappedSpingoClostridiumGroupScoreQ3 = quantile(spingoClostridiumGroupScore, 0.75),
              mappedSpingoClostridiumGroupScoreIQR = IQR(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreMAD = mad(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreNonNormalMAD = mad(spingoClostridiumGroupScore, constant = 1 / quantile(spingoClostridiumGroupScore, 0.75)),
              mappedSpingoGenusScoreMean = mean(spingoGenusScore),
              mappedSpingoGenusScoreSD = sd(spingoGenusScore),
              mappedSpingoGenusScoreCI = qnorm(0.975) * sd(spingoGenusScore) / sqrt(n()),
              mappedSpingoGenusScoreMedian = median(spingoGenusScore),
              mappedSpingoGenusScoreQ1 = quantile(spingoGenusScore, 0.25),
              mappedSpingoGenusScoreQ3 = quantile(spingoGenusScore, 0.75),
              mappedSpingoGenusScoreIQR = IQR(spingoGenusScore),
              mappedSpingoGenusScoreMAD = mad(spingoGenusScore),
              mappedSpingoGenusScoreNonNormalMAD = mad(spingoGenusScore, constant = 1 / quantile(spingoGenusScore, 0.75)),
              mappedSpingoSpeciesScoreMean = mean(spingoSpeciesScore),
              mappedSpingoSpeciesScoreSD = sd(spingoSpeciesScore),
              mappedSpingoSpeciesScoreCI = qnorm(0.975) * sd(spingoSpeciesScore) / sqrt(n()),
              mappedSpingoSpeciesScoreMedian = median(spingoSpeciesScore),
              mappedSpingoSpeciesScoreQ1 = quantile(spingoSpeciesScore, 0.25),
              mappedSpingoSpeciesScoreQ3 = quantile(spingoSpeciesScore, 0.75),
              mappedSpingoSpeciesScoreIQR = IQR(spingoSpeciesScore),
              mappedSpingoSpeciesScoreMAD = mad(spingoSpeciesScore),
              mappedSpingoSpeciesScoreNonNormalMAD = mad(spingoSpeciesScore, constant = 1 / quantile(spingoSpeciesScore, 0.75))
    ) %>%
    rename(standardId = classId) %>%
    mutate(rank = "class")
  return(classClassificationsSummaries)
}

.getPhylumMappedSpingoClassificationsSummaries <- function(spingoClassifications = NULL) {
  phylumClassificationsSummaries <- spingoClassifications %>%
    subset(phylumId %>% is.na == F) %>%
    group_by(phylumId, spingoClostridiumGroup) %>%
    summarize(mappedSpingoNumberOfSequences = n(),
              mappedSpingoScoreMean = mean(spingoScore),
              mappedSpingoScoreSD = sd(spingoScore),
              mappedSpingoScoreCI = qnorm(0.975) * sd(spingoScore) / sqrt(n()),
              mappedSpingoScoreMedian = median(spingoScore),
              mappedSpingoScoreQ1 = quantile(spingoScore, 0.25),
              mappedSpingoScoreQ3 = quantile(spingoScore, 0.75),
              mappedSpingoScoreIQR = IQR(spingoScore),
              mappedSpingoScoreMAD = mad(spingoScore),
              mappedSpingoScoreNonNormalMAD = mad(spingoScore, constant = 1 / quantile(spingoScore, 0.75)),
              mappedSpingoClostridiumGroupScoreMean = mean(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreSD = sd(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreCI = qnorm(0.975) * sd(spingoClostridiumGroupScore) / sqrt(n()),
              mappedSpingoClostridiumGroupScoreMedian = median(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreQ1 = quantile(spingoClostridiumGroupScore, 0.25),
              mappedSpingoClostridiumGroupScoreQ3 = quantile(spingoClostridiumGroupScore, 0.75),
              mappedSpingoClostridiumGroupScoreIQR = IQR(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreMAD = mad(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreNonNormalMAD = mad(spingoClostridiumGroupScore, constant = 1 / quantile(spingoClostridiumGroupScore, 0.75)),
              mappedSpingoGenusScoreMean = mean(spingoGenusScore),
              mappedSpingoGenusScoreSD = sd(spingoGenusScore),
              mappedSpingoGenusScoreCI = qnorm(0.975) * sd(spingoGenusScore) / sqrt(n()),
              mappedSpingoGenusScoreMedian = median(spingoGenusScore),
              mappedSpingoGenusScoreQ1 = quantile(spingoGenusScore, 0.25),
              mappedSpingoGenusScoreQ3 = quantile(spingoGenusScore, 0.75),
              mappedSpingoGenusScoreIQR = IQR(spingoGenusScore),
              mappedSpingoGenusScoreMAD = mad(spingoGenusScore),
              mappedSpingoGenusScoreNonNormalMAD = mad(spingoGenusScore, constant = 1 / quantile(spingoGenusScore, 0.75)),
              mappedSpingoSpeciesScoreMean = mean(spingoSpeciesScore),
              mappedSpingoSpeciesScoreSD = sd(spingoSpeciesScore),
              mappedSpingoSpeciesScoreCI = qnorm(0.975) * sd(spingoSpeciesScore) / sqrt(n()),
              mappedSpingoSpeciesScoreMedian = median(spingoSpeciesScore),
              mappedSpingoSpeciesScoreQ1 = quantile(spingoSpeciesScore, 0.25),
              mappedSpingoSpeciesScoreQ3 = quantile(spingoSpeciesScore, 0.75),
              mappedSpingoSpeciesScoreIQR = IQR(spingoSpeciesScore),
              mappedSpingoSpeciesScoreMAD = mad(spingoSpeciesScore),
              mappedSpingoSpeciesScoreNonNormalMAD = mad(spingoSpeciesScore, constant = 1 / quantile(spingoSpeciesScore, 0.75))
    ) %>%
    rename(standardId = phylumId) %>%
    mutate(rank = "phylum")
  return(phylumClassificationsSummaries)
}

.getSuperkingdomMappedSpingoClassificationsSummaries <- function(spingoClassifications = NULL) {
  superkingdomClassificationsSummaries <- spingoClassifications %>%
    subset(superkingdomId %>% is.na == F) %>%
    group_by(superkingdomId, spingoClostridiumGroup) %>%
    summarize(mappedSpingoNumberOfSequences = n(),
              mappedSpingoScoreMean = mean(spingoScore),
              mappedSpingoScoreSD = sd(spingoScore),
              mappedSpingoScoreCI = qnorm(0.975) * sd(spingoScore) / sqrt(n()),
              mappedSpingoScoreMedian = median(spingoScore),
              mappedSpingoScoreQ1 = quantile(spingoScore, 0.25),
              mappedSpingoScoreQ3 = quantile(spingoScore, 0.75),
              mappedSpingoScoreIQR = IQR(spingoScore),
              mappedSpingoScoreMAD = mad(spingoScore),
              mappedSpingoScoreNonNormalMAD = mad(spingoScore, constant = 1 / quantile(spingoScore, 0.75)),
              mappedSpingoClostridiumGroupScoreMean = mean(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreSD = sd(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreCI = qnorm(0.975) * sd(spingoClostridiumGroupScore) / sqrt(n()),
              mappedSpingoClostridiumGroupScoreMedian = median(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreQ1 = quantile(spingoClostridiumGroupScore, 0.25),
              mappedSpingoClostridiumGroupScoreQ3 = quantile(spingoClostridiumGroupScore, 0.75),
              mappedSpingoClostridiumGroupScoreIQR = IQR(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreMAD = mad(spingoClostridiumGroupScore),
              mappedSpingoClostridiumGroupScoreNonNormalMAD = mad(spingoClostridiumGroupScore, constant = 1 / quantile(spingoClostridiumGroupScore, 0.75)),
              mappedSpingoGenusScoreMean = mean(spingoGenusScore),
              mappedSpingoGenusScoreSD = sd(spingoGenusScore),
              mappedSpingoGenusScoreCI = qnorm(0.975) * sd(spingoGenusScore) / sqrt(n()),
              mappedSpingoGenusScoreMedian = median(spingoGenusScore),
              mappedSpingoGenusScoreQ1 = quantile(spingoGenusScore, 0.25),
              mappedSpingoGenusScoreQ3 = quantile(spingoGenusScore, 0.75),
              mappedSpingoGenusScoreIQR = IQR(spingoGenusScore),
              mappedSpingoGenusScoreMAD = mad(spingoGenusScore),
              mappedSpingoGenusScoreNonNormalMAD = mad(spingoGenusScore, constant = 1 / quantile(spingoGenusScore, 0.75)),
              mappedSpingoSpeciesScoreMean = mean(spingoSpeciesScore),
              mappedSpingoSpeciesScoreSD = sd(spingoSpeciesScore),
              mappedSpingoSpeciesScoreCI = qnorm(0.975) * sd(spingoSpeciesScore) / sqrt(n()),
              mappedSpingoSpeciesScoreMedian = median(spingoSpeciesScore),
              mappedSpingoSpeciesScoreQ1 = quantile(spingoSpeciesScore, 0.25),
              mappedSpingoSpeciesScoreQ3 = quantile(spingoSpeciesScore, 0.75),
              mappedSpingoSpeciesScoreIQR = IQR(spingoSpeciesScore),
              mappedSpingoSpeciesScoreMAD = mad(spingoSpeciesScore),
              mappedSpingoSpeciesScoreNonNormalMAD = mad(spingoSpeciesScore, constant = 1 / quantile(spingoSpeciesScore, 0.75))
    ) %>%
    rename(standardId = superkingdomId) %>%
    mutate(rank = "superkingdom")
  return(superkingdomClassificationsSummaries)
}

#'
#'
#' @param spingoClassifications
summarizeSpingoClassifications <- function(spingoClassifications = NULL) {
  directClassificationsSummaries <- .getDirectSpingoClassificationsSummaries(spingoClassifications)

  superkingdomClassificationsSummaries <- .getSuperkingdomMappedSpingoClassificationsSummaries(spingoClassifications)
  phylumClassificationsSummaries <- .getPhylumMappedSpingoClassificationsSummaries(spingoClassifications)
  classClassificationsSummaries <- .getClassMappedSpingoClassificationsSummaries(spingoClassifications)
  orderClassificationsSummaries <- .getOrderMappedSpingoClassificationsSummaries(spingoClassifications)
  familyClassificationsSummaries <- .getFamilyMappedSpingoClassificationsSummaries(spingoClassifications)
  genusClassificationsSummaries <- .getGenusMappedSpingoClassificationsSummaries(spingoClassifications)
  speciesClassificationsSummaries <- .getSpeciesMappedSpingoClassificationsSummaries(spingoClassifications)

  mappedClassificationsSummaries <- rbind(
    superkingdomClassificationsSummaries,
    phylumClassificationsSummaries,
    classClassificationsSummaries,
    orderClassificationsSummaries,
    familyClassificationsSummaries,
    genusClassificationsSummaries,
    speciesClassificationsSummaries
  )

  spingoClassificationsSummaries <- directClassificationsSummaries %>%
    merge(mappedClassificationsSummaries, by = c("rank", "standardId", "spingoClostridiumGroup"), all = T)

  return(spingoClassificationsSummaries)
}
