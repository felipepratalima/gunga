require(magrittr)
require(dplyr)

# .getSpeciesMappedQiimeClassificationsSummaries <- function(qiimeClassifications = NULL) {
#   speciesClassificationsSummaries <- qiimeClassifications %>%
#     subset(speciesId %>% is.na == F) %>%
#     group_by(speciesId) %>%
#     summarize(mappedSpingoNumberOfSequences = n(),
#               mappedSpingoScoreMean = mean(spingoScore),
#               mappedSpingoScoreSD = sd(spingoScore),
#               mappedSpingoScoreCI = qnorm(0.975) * sd(spingoScore) / sqrt(n()),
#               mappedSpingoScoreMedian = median(spingoScore),
#               mappedSpingoScoreQ1 = quantile(spingoScore, 0.25),
#               mappedSpingoScoreQ3 = quantile(spingoScore, 0.75),
#               mappedSpingoScoreIQR = IQR(spingoScore),
#               mappedSpingoScoreMAD = mad(spingoScore),
#               mappedSpingoScoreNonNormalMAD = mad(spingoScore, constant = 1 / quantile(spingoScore, 0.75)),
#               mappedSpingoClostridiumGroupScoreMean = mean(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreSD = sd(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreCI = qnorm(0.975) * sd(spingoClostridiumGroupScore) / sqrt(n()),
#               mappedSpingoClostridiumGroupScoreMedian = median(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreQ1 = quantile(spingoClostridiumGroupScore, 0.25),
#               mappedSpingoClostridiumGroupScoreQ3 = quantile(spingoClostridiumGroupScore, 0.75),
#               mappedSpingoClostridiumGroupScoreIQR = IQR(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreMAD = mad(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreNonNormalMAD = mad(spingoClostridiumGroupScore, constant = 1 / quantile(spingoClostridiumGroupScore, 0.75)),
#               mappedSpingoGenusScoreMean = mean(spingoGenusScore),
#               mappedSpingoGenusScoreSD = sd(spingoGenusScore),
#               mappedSpingoGenusScoreCI = qnorm(0.975) * sd(spingoGenusScore) / sqrt(n()),
#               mappedSpingoGenusScoreMedian = median(spingoGenusScore),
#               mappedSpingoGenusScoreQ1 = quantile(spingoGenusScore, 0.25),
#               mappedSpingoGenusScoreQ3 = quantile(spingoGenusScore, 0.75),
#               mappedSpingoGenusScoreIQR = IQR(spingoGenusScore),
#               mappedSpingoGenusScoreMAD = mad(spingoGenusScore),
#               mappedSpingoGenusScoreNonNormalMAD = mad(spingoGenusScore, constant = 1 / quantile(spingoGenusScore, 0.75)),
#               mappedSpingoSpeciesScoreMean = mean(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreSD = sd(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreCI = qnorm(0.975) * sd(spingoSpeciesScore) / sqrt(n()),
#               mappedSpingoSpeciesScoreMedian = median(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreQ1 = quantile(spingoSpeciesScore, 0.25),
#               mappedSpingoSpeciesScoreQ3 = quantile(spingoSpeciesScore, 0.75),
#               mappedSpingoSpeciesScoreIQR = IQR(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreMAD = mad(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreNonNormalMAD = mad(spingoSpeciesScore, constant = 1 / quantile(spingoSpeciesScore, 0.75))
#     ) %>%
#     rename(standardId = speciesId) %>%
#     mutate(rank = "species")
#
#   ## convert to char, beacuse sometimes we have non recognized taxonomies
#   speciesClassificationsSummaries$standardId <- speciesClassificationsSummaries$standardId %>% as.character
#
#   return(speciesClassificationsSummaries)
# }
#
# .getGenusMappedQiimeClassificationsSummaries <- function(qiimeClassifications = NULL) {
#   genusClassificationsSummaries <- qiimeClassifications %>%
#     subset(genusId %>% is.na == F) %>%
#     group_by(genusId) %>%
#     summarize(mappedSpingoNumberOfSequences = n(),
#               mappedSpingoScoreMean = mean(spingoScore),
#               mappedSpingoScoreSD = sd(spingoScore),
#               mappedSpingoScoreCI = qnorm(0.975) * sd(spingoScore) / sqrt(n()),
#               mappedSpingoScoreMedian = median(spingoScore),
#               mappedSpingoScoreQ1 = quantile(spingoScore, 0.25),
#               mappedSpingoScoreQ3 = quantile(spingoScore, 0.75),
#               mappedSpingoScoreIQR = IQR(spingoScore),
#               mappedSpingoScoreMAD = mad(spingoScore),
#               mappedSpingoScoreNonNormalMAD = mad(spingoScore, constant = 1 / quantile(spingoScore, 0.75)),
#               mappedSpingoClostridiumGroupScoreMean = mean(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreSD = sd(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreCI = qnorm(0.975) * sd(spingoClostridiumGroupScore) / sqrt(n()),
#               mappedSpingoClostridiumGroupScoreMedian = median(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreQ1 = quantile(spingoClostridiumGroupScore, 0.25),
#               mappedSpingoClostridiumGroupScoreQ3 = quantile(spingoClostridiumGroupScore, 0.75),
#               mappedSpingoClostridiumGroupScoreIQR = IQR(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreMAD = mad(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreNonNormalMAD = mad(spingoClostridiumGroupScore, constant = 1 / quantile(spingoClostridiumGroupScore, 0.75)),
#               mappedSpingoGenusScoreMean = mean(spingoGenusScore),
#               mappedSpingoGenusScoreSD = sd(spingoGenusScore),
#               mappedSpingoGenusScoreCI = qnorm(0.975) * sd(spingoGenusScore) / sqrt(n()),
#               mappedSpingoGenusScoreMedian = median(spingoGenusScore),
#               mappedSpingoGenusScoreQ1 = quantile(spingoGenusScore, 0.25),
#               mappedSpingoGenusScoreQ3 = quantile(spingoGenusScore, 0.75),
#               mappedSpingoGenusScoreIQR = IQR(spingoGenusScore),
#               mappedSpingoGenusScoreMAD = mad(spingoGenusScore),
#               mappedSpingoGenusScoreNonNormalMAD = mad(spingoGenusScore, constant = 1 / quantile(spingoGenusScore, 0.75)),
#               mappedSpingoSpeciesScoreMean = mean(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreSD = sd(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreCI = qnorm(0.975) * sd(spingoSpeciesScore) / sqrt(n()),
#               mappedSpingoSpeciesScoreMedian = median(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreQ1 = quantile(spingoSpeciesScore, 0.25),
#               mappedSpingoSpeciesScoreQ3 = quantile(spingoSpeciesScore, 0.75),
#               mappedSpingoSpeciesScoreIQR = IQR(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreMAD = mad(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreNonNormalMAD = mad(spingoSpeciesScore, constant = 1 / quantile(spingoSpeciesScore, 0.75))
#     ) %>%
#     rename(standardId = genusId) %>%
#     mutate(rank = "genus")
#
#   ## convert to char, beacuse sometimes we have non recognized taxonomies
#   genusClassificationsSummaries$standardId <- genusClassificationsSummaries$standardId %>% as.character
#
#   return(genusClassificationsSummaries)
# }
#
# .getFamilyMappedQiimeClassificationsSummaries <- function(qiimeClassifications = NULL) {
#   familyClassificationsSummaries <- qiimeClassifications %>%
#     subset(familyId %>% is.na == F) %>%
#     group_by(familyId) %>%
#     summarize(mappedSpingoNumberOfSequences = n(),
#               mappedSpingoScoreMean = mean(spingoScore),
#               mappedSpingoScoreSD = sd(spingoScore),
#               mappedSpingoScoreCI = qnorm(0.975) * sd(spingoScore) / sqrt(n()),
#               mappedSpingoScoreMedian = median(spingoScore),
#               mappedSpingoScoreQ1 = quantile(spingoScore, 0.25),
#               mappedSpingoScoreQ3 = quantile(spingoScore, 0.75),
#               mappedSpingoScoreIQR = IQR(spingoScore),
#               mappedSpingoScoreMAD = mad(spingoScore),
#               mappedSpingoScoreNonNormalMAD = mad(spingoScore, constant = 1 / quantile(spingoScore, 0.75)),
#               mappedSpingoClostridiumGroupScoreMean = mean(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreSD = sd(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreCI = qnorm(0.975) * sd(spingoClostridiumGroupScore) / sqrt(n()),
#               mappedSpingoClostridiumGroupScoreMedian = median(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreQ1 = quantile(spingoClostridiumGroupScore, 0.25),
#               mappedSpingoClostridiumGroupScoreQ3 = quantile(spingoClostridiumGroupScore, 0.75),
#               mappedSpingoClostridiumGroupScoreIQR = IQR(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreMAD = mad(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreNonNormalMAD = mad(spingoClostridiumGroupScore, constant = 1 / quantile(spingoClostridiumGroupScore, 0.75)),
#               mappedSpingoGenusScoreMean = mean(spingoGenusScore),
#               mappedSpingoGenusScoreSD = sd(spingoGenusScore),
#               mappedSpingoGenusScoreCI = qnorm(0.975) * sd(spingoGenusScore) / sqrt(n()),
#               mappedSpingoGenusScoreMedian = median(spingoGenusScore),
#               mappedSpingoGenusScoreQ1 = quantile(spingoGenusScore, 0.25),
#               mappedSpingoGenusScoreQ3 = quantile(spingoGenusScore, 0.75),
#               mappedSpingoGenusScoreIQR = IQR(spingoGenusScore),
#               mappedSpingoGenusScoreMAD = mad(spingoGenusScore),
#               mappedSpingoGenusScoreNonNormalMAD = mad(spingoGenusScore, constant = 1 / quantile(spingoGenusScore, 0.75)),
#               mappedSpingoSpeciesScoreMean = mean(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreSD = sd(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreCI = qnorm(0.975) * sd(spingoSpeciesScore) / sqrt(n()),
#               mappedSpingoSpeciesScoreMedian = median(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreQ1 = quantile(spingoSpeciesScore, 0.25),
#               mappedSpingoSpeciesScoreQ3 = quantile(spingoSpeciesScore, 0.75),
#               mappedSpingoSpeciesScoreIQR = IQR(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreMAD = mad(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreNonNormalMAD = mad(spingoSpeciesScore, constant = 1 / quantile(spingoSpeciesScore, 0.75))
#     ) %>%
#     rename(standardId = familyId) %>%
#     mutate(rank = "family")
#
#   ## convert to char, beacuse sometimes we have non recognized taxonomies
#   familyClassificationsSummaries$standardId <- familyClassificationsSummaries$standardId %>% as.character
#
#   return(familyClassificationsSummaries)
# }
#
# .getOrderMappedQiimeClassificationsSummaries <- function(qiimeClassifications = NULL) {
#   orderClassificationsSummaries <- qiimeClassifications %>%
#     subset(orderId %>% is.na == F) %>%
#     group_by(orderId) %>%
#     summarize(mappedSpingoNumberOfSequences = n(),
#               mappedSpingoScoreMean = mean(spingoScore),
#               mappedSpingoScoreSD = sd(spingoScore),
#               mappedSpingoScoreCI = qnorm(0.975) * sd(spingoScore) / sqrt(n()),
#               mappedSpingoScoreMedian = median(spingoScore),
#               mappedSpingoScoreQ1 = quantile(spingoScore, 0.25),
#               mappedSpingoScoreQ3 = quantile(spingoScore, 0.75),
#               mappedSpingoScoreIQR = IQR(spingoScore),
#               mappedSpingoScoreMAD = mad(spingoScore),
#               mappedSpingoScoreNonNormalMAD = mad(spingoScore, constant = 1 / quantile(spingoScore, 0.75)),
#               mappedSpingoClostridiumGroupScoreMean = mean(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreSD = sd(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreCI = qnorm(0.975) * sd(spingoClostridiumGroupScore) / sqrt(n()),
#               mappedSpingoClostridiumGroupScoreMedian = median(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreQ1 = quantile(spingoClostridiumGroupScore, 0.25),
#               mappedSpingoClostridiumGroupScoreQ3 = quantile(spingoClostridiumGroupScore, 0.75),
#               mappedSpingoClostridiumGroupScoreIQR = IQR(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreMAD = mad(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreNonNormalMAD = mad(spingoClostridiumGroupScore, constant = 1 / quantile(spingoClostridiumGroupScore, 0.75)),
#               mappedSpingoGenusScoreMean = mean(spingoGenusScore),
#               mappedSpingoGenusScoreSD = sd(spingoGenusScore),
#               mappedSpingoGenusScoreCI = qnorm(0.975) * sd(spingoGenusScore) / sqrt(n()),
#               mappedSpingoGenusScoreMedian = median(spingoGenusScore),
#               mappedSpingoGenusScoreQ1 = quantile(spingoGenusScore, 0.25),
#               mappedSpingoGenusScoreQ3 = quantile(spingoGenusScore, 0.75),
#               mappedSpingoGenusScoreIQR = IQR(spingoGenusScore),
#               mappedSpingoGenusScoreMAD = mad(spingoGenusScore),
#               mappedSpingoGenusScoreNonNormalMAD = mad(spingoGenusScore, constant = 1 / quantile(spingoGenusScore, 0.75)),
#               mappedSpingoSpeciesScoreMean = mean(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreSD = sd(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreCI = qnorm(0.975) * sd(spingoSpeciesScore) / sqrt(n()),
#               mappedSpingoSpeciesScoreMedian = median(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreQ1 = quantile(spingoSpeciesScore, 0.25),
#               mappedSpingoSpeciesScoreQ3 = quantile(spingoSpeciesScore, 0.75),
#               mappedSpingoSpeciesScoreIQR = IQR(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreMAD = mad(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreNonNormalMAD = mad(spingoSpeciesScore, constant = 1 / quantile(spingoSpeciesScore, 0.75))
#     ) %>%
#     rename(standardId = orderId) %>%
#     mutate(rank = "order")
#
#   ## convert to char, beacuse sometimes we have non recognized taxonomies
#   orderClassificationsSummaries$standardId <- orderClassificationsSummaries$standardId %>% as.character
#
#   return(orderClassificationsSummaries)
# }
#
# .getClassMappedQiimeClassificationsSummaries <- function(qiimeClassifications = NULL) {
#   classClassificationsSummaries <- qiimeClassifications %>%
#     subset(classId %>% is.na == F) %>%
#     group_by(classId) %>%
#     summarize(mappedSpingoNumberOfSequences = n(),
#               mappedSpingoScoreMean = mean(spingoScore),
#               mappedSpingoScoreSD = sd(spingoScore),
#               mappedSpingoScoreCI = qnorm(0.975) * sd(spingoScore) / sqrt(n()),
#               mappedSpingoScoreMedian = median(spingoScore),
#               mappedSpingoScoreQ1 = quantile(spingoScore, 0.25),
#               mappedSpingoScoreQ3 = quantile(spingoScore, 0.75),
#               mappedSpingoScoreIQR = IQR(spingoScore),
#               mappedSpingoScoreMAD = mad(spingoScore),
#               mappedSpingoScoreNonNormalMAD = mad(spingoScore, constant = 1 / quantile(spingoScore, 0.75)),
#               mappedSpingoClostridiumGroupScoreMean = mean(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreSD = sd(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreCI = qnorm(0.975) * sd(spingoClostridiumGroupScore) / sqrt(n()),
#               mappedSpingoClostridiumGroupScoreMedian = median(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreQ1 = quantile(spingoClostridiumGroupScore, 0.25),
#               mappedSpingoClostridiumGroupScoreQ3 = quantile(spingoClostridiumGroupScore, 0.75),
#               mappedSpingoClostridiumGroupScoreIQR = IQR(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreMAD = mad(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreNonNormalMAD = mad(spingoClostridiumGroupScore, constant = 1 / quantile(spingoClostridiumGroupScore, 0.75)),
#               mappedSpingoGenusScoreMean = mean(spingoGenusScore),
#               mappedSpingoGenusScoreSD = sd(spingoGenusScore),
#               mappedSpingoGenusScoreCI = qnorm(0.975) * sd(spingoGenusScore) / sqrt(n()),
#               mappedSpingoGenusScoreMedian = median(spingoGenusScore),
#               mappedSpingoGenusScoreQ1 = quantile(spingoGenusScore, 0.25),
#               mappedSpingoGenusScoreQ3 = quantile(spingoGenusScore, 0.75),
#               mappedSpingoGenusScoreIQR = IQR(spingoGenusScore),
#               mappedSpingoGenusScoreMAD = mad(spingoGenusScore),
#               mappedSpingoGenusScoreNonNormalMAD = mad(spingoGenusScore, constant = 1 / quantile(spingoGenusScore, 0.75)),
#               mappedSpingoSpeciesScoreMean = mean(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreSD = sd(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreCI = qnorm(0.975) * sd(spingoSpeciesScore) / sqrt(n()),
#               mappedSpingoSpeciesScoreMedian = median(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreQ1 = quantile(spingoSpeciesScore, 0.25),
#               mappedSpingoSpeciesScoreQ3 = quantile(spingoSpeciesScore, 0.75),
#               mappedSpingoSpeciesScoreIQR = IQR(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreMAD = mad(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreNonNormalMAD = mad(spingoSpeciesScore, constant = 1 / quantile(spingoSpeciesScore, 0.75))
#     ) %>%
#     rename(standardId = classId) %>%
#     mutate(rank = "class")
#
#   ## convert to char, beacuse sometimes we have non recognized taxonomies
#   classClassificationsSummaries$standardId <- classClassificationsSummaries$standardId %>% as.character
#
#   return(classClassificationsSummaries)
# }
#
# .getPhylumMappedQiimeClassificationsSummaries <- function(qiimeClassifications = NULL) {
#   phylumClassificationsSummaries <- qiimeClassifications %>%
#     subset(phylumId %>% is.na == F) %>%
#     group_by(phylumId) %>%
#     summarize(mappedSpingoNumberOfSequences = n(),
#               mappedSpingoScoreMean = mean(spingoScore),
#               mappedSpingoScoreSD = sd(spingoScore),
#               mappedSpingoScoreCI = qnorm(0.975) * sd(spingoScore) / sqrt(n()),
#               mappedSpingoScoreMedian = median(spingoScore),
#               mappedSpingoScoreQ1 = quantile(spingoScore, 0.25),
#               mappedSpingoScoreQ3 = quantile(spingoScore, 0.75),
#               mappedSpingoScoreIQR = IQR(spingoScore),
#               mappedSpingoScoreMAD = mad(spingoScore),
#               mappedSpingoScoreNonNormalMAD = mad(spingoScore, constant = 1 / quantile(spingoScore, 0.75)),
#               mappedSpingoClostridiumGroupScoreMean = mean(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreSD = sd(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreCI = qnorm(0.975) * sd(spingoClostridiumGroupScore) / sqrt(n()),
#               mappedSpingoClostridiumGroupScoreMedian = median(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreQ1 = quantile(spingoClostridiumGroupScore, 0.25),
#               mappedSpingoClostridiumGroupScoreQ3 = quantile(spingoClostridiumGroupScore, 0.75),
#               mappedSpingoClostridiumGroupScoreIQR = IQR(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreMAD = mad(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreNonNormalMAD = mad(spingoClostridiumGroupScore, constant = 1 / quantile(spingoClostridiumGroupScore, 0.75)),
#               mappedSpingoGenusScoreMean = mean(spingoGenusScore),
#               mappedSpingoGenusScoreSD = sd(spingoGenusScore),
#               mappedSpingoGenusScoreCI = qnorm(0.975) * sd(spingoGenusScore) / sqrt(n()),
#               mappedSpingoGenusScoreMedian = median(spingoGenusScore),
#               mappedSpingoGenusScoreQ1 = quantile(spingoGenusScore, 0.25),
#               mappedSpingoGenusScoreQ3 = quantile(spingoGenusScore, 0.75),
#               mappedSpingoGenusScoreIQR = IQR(spingoGenusScore),
#               mappedSpingoGenusScoreMAD = mad(spingoGenusScore),
#               mappedSpingoGenusScoreNonNormalMAD = mad(spingoGenusScore, constant = 1 / quantile(spingoGenusScore, 0.75)),
#               mappedSpingoSpeciesScoreMean = mean(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreSD = sd(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreCI = qnorm(0.975) * sd(spingoSpeciesScore) / sqrt(n()),
#               mappedSpingoSpeciesScoreMedian = median(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreQ1 = quantile(spingoSpeciesScore, 0.25),
#               mappedSpingoSpeciesScoreQ3 = quantile(spingoSpeciesScore, 0.75),
#               mappedSpingoSpeciesScoreIQR = IQR(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreMAD = mad(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreNonNormalMAD = mad(spingoSpeciesScore, constant = 1 / quantile(spingoSpeciesScore, 0.75))
#     ) %>%
#     rename(standardId = phylumId) %>%
#     mutate(rank = "phylum")
#
#   ## convert to char, beacuse sometimes we have non recognized taxonomies
#   phylumClassificationsSummaries$standardId <- phylumClassificationsSummaries$standardId %>% as.character
#
#   return(phylumClassificationsSummaries)
# }
#
# .getSuperkingdomMappedQiimeClassificationsSummaries <- function(qiimeClassifications = NULL) {
#   superkingdomClassificationsSummaries <- qiimeClassifications %>%
#     subset(superkingdomId %>% is.na == F) %>%
#     group_by(superkingdomId) %>%
#     summarize(mappedSpingoNumberOfSequences = n(),
#               mappedSpingoScoreMean = mean(spingoScore),
#               mappedSpingoScoreSD = sd(spingoScore),
#               mappedSpingoScoreCI = qnorm(0.975) * sd(spingoScore) / sqrt(n()),
#               mappedSpingoScoreMedian = median(spingoScore),
#               mappedSpingoScoreQ1 = quantile(spingoScore, 0.25),
#               mappedSpingoScoreQ3 = quantile(spingoScore, 0.75),
#               mappedSpingoScoreIQR = IQR(spingoScore),
#               mappedSpingoScoreMAD = mad(spingoScore),
#               mappedSpingoScoreNonNormalMAD = mad(spingoScore, constant = 1 / quantile(spingoScore, 0.75)),
#               mappedSpingoClostridiumGroupScoreMean = mean(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreSD = sd(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreCI = qnorm(0.975) * sd(spingoClostridiumGroupScore) / sqrt(n()),
#               mappedSpingoClostridiumGroupScoreMedian = median(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreQ1 = quantile(spingoClostridiumGroupScore, 0.25),
#               mappedSpingoClostridiumGroupScoreQ3 = quantile(spingoClostridiumGroupScore, 0.75),
#               mappedSpingoClostridiumGroupScoreIQR = IQR(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreMAD = mad(spingoClostridiumGroupScore),
#               mappedSpingoClostridiumGroupScoreNonNormalMAD = mad(spingoClostridiumGroupScore, constant = 1 / quantile(spingoClostridiumGroupScore, 0.75)),
#               mappedSpingoGenusScoreMean = mean(spingoGenusScore),
#               mappedSpingoGenusScoreSD = sd(spingoGenusScore),
#               mappedSpingoGenusScoreCI = qnorm(0.975) * sd(spingoGenusScore) / sqrt(n()),
#               mappedSpingoGenusScoreMedian = median(spingoGenusScore),
#               mappedSpingoGenusScoreQ1 = quantile(spingoGenusScore, 0.25),
#               mappedSpingoGenusScoreQ3 = quantile(spingoGenusScore, 0.75),
#               mappedSpingoGenusScoreIQR = IQR(spingoGenusScore),
#               mappedSpingoGenusScoreMAD = mad(spingoGenusScore),
#               mappedSpingoGenusScoreNonNormalMAD = mad(spingoGenusScore, constant = 1 / quantile(spingoGenusScore, 0.75)),
#               mappedSpingoSpeciesScoreMean = mean(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreSD = sd(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreCI = qnorm(0.975) * sd(spingoSpeciesScore) / sqrt(n()),
#               mappedSpingoSpeciesScoreMedian = median(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreQ1 = quantile(spingoSpeciesScore, 0.25),
#               mappedSpingoSpeciesScoreQ3 = quantile(spingoSpeciesScore, 0.75),
#               mappedSpingoSpeciesScoreIQR = IQR(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreMAD = mad(spingoSpeciesScore),
#               mappedSpingoSpeciesScoreNonNormalMAD = mad(spingoSpeciesScore, constant = 1 / quantile(spingoSpeciesScore, 0.75))
#     ) %>%
#     rename(standardId = superkingdomId) %>%
#     mutate(rank = "superkingdom")
#
#   ## convert to char, beacuse sometimes we have non recognized taxonomies
#   superkingdomClassificationsSummaries$standardId <- superkingdomClassificationsSummaries$standardId %>% as.character
#
#   return(superkingdomClassificationsSummaries)
# }

.getDirectQiimeClassificationsSummaries <- function(qiimeClassifications = NULL) {
  directClassificationsSummaries <- qiimeClassifications %>%
    subset(standardId %>% is.na == F) %>%
    group_by(rank, standardId) %>%
    summarize(directQiimeNumberOfSequences = n(),
              directQiimeScoreMean = mean(qiimeScore),
              directQiimeScoreSD = sd(qiimeScore),
              directQiimeScoreCI = qnorm(0.975) * sd(qiimeScore) / sqrt(n()),
              directQiimeScoreMedian = median(qiimeScore),
              directQiimeScoreQ1 = quantile(qiimeScore, 0.25),
              directQiimeScoreQ3 = quantile(qiimeScore, 0.75),
              directQiimeScoreIQR = IQR(qiimeScore),
              directQiimeScoreMAD = mad(qiimeScore),
              directQiimeScoreNonNormalMAD = mad(qiimeScore, constant = 1 / quantile(qiimeScore, 0.75)),
              directQiimeNumberOfSequencesMean = mean(qiimeNumberOfReads),
              directQiimeNumberOfSequencesSD = sd(qiimeNumberOfReads),
              directQiimeNumberOfSequencesCI = qnorm(0.975) * sd(qiimeNumberOfReads) / sqrt(n()),
              directQiimeNumberOfSequencesMedian = median(qiimeNumberOfReads),
              directQiimeNumberOfSequencesQ1 = quantile(qiimeNumberOfReads, 0.25),
              directQiimeNumberOfSequencesQ3 = quantile(qiimeNumberOfReads, 0.75),
              directQiimeNumberOfSequencesIQR = IQR(qiimeNumberOfReads),
              directQiimeNumberOfSequencesMAD = mad(qiimeNumberOfReads),
              directQiimeNumberOfSequencesNonNormalMAD = mad(qiimeNumberOfReads, constant = 1 / quantile(qiimeNumberOfReads, 0.75))
    )

  ## convert to char, beacuse sometimes we have non recognized taxonomies
  directClassificationsSummaries$standardId <- directClassificationsSummaries$standardId %>% as.character

  return(directClassificationsSummaries)
}

.getMappeQiimeClassificationsClassificationsSummariesByRank <- function(qiimeClassifications = NULL, taxonomyRank = "species") {
  summaryFieldName <- taxonomyRank %>% paste0("Id")
  summaryIndexes <- qiimeClassifications[[summaryFieldName]] %>% is.na == F
  classificationsSummaries <-
    qiimeClassifications %>%
    subset(summaryIndexes) %>%
    group_by_(summaryFieldName) %>%
    summarize(mappedQiimeNumberOfSequences = n(),
              mappedQiimeScoreMean = mean(qiimeScore),
              mappedQiimeScoreSD = sd(qiimeScore),
              mappedQiimeScoreCI = qnorm(0.975) * sd(qiimeScore) / sqrt(n()),
              mappedQiimeScoreMedian = median(qiimeScore),
              mappedQiimeScoreQ1 = quantile(qiimeScore, 0.25),
              mappedQiimeScoreQ3 = quantile(qiimeScore, 0.75),
              mappedQiimeScoreIQR = IQR(qiimeScore),
              mappedQiimeScoreMAD = mad(qiimeScore),
              mappedQiimeScoreNonNormalMAD = mad(qiimeScore, constant = 1 / quantile(qiimeScore, 0.75)),
              mappedQiimeNumberOfSequencesMean = mean(qiimeNumberOfReads),
              mappedQiimeNumberOfSequencesSD = sd(qiimeNumberOfReads),
              mappedQiimeNumberOfSequencesCI = qnorm(0.975) * sd(qiimeNumberOfReads) / sqrt(n()),
              mappedQiimeNumberOfSequencesMedian = median(qiimeNumberOfReads),
              mappedQiimeNumberOfSequencesQ1 = quantile(qiimeNumberOfReads, 0.25),
              mappedQiimeNumberOfSequencesQ3 = quantile(qiimeNumberOfReads, 0.75),
              mappedQiimeNumberOfSequencesIQR = IQR(qiimeNumberOfReads),
              mappedQiimeNumberOfSequencesMAD = mad(qiimeNumberOfReads),
              mappedQiimeNumberOfSequencesNonNormalMAD = mad(qiimeNumberOfReads, constant = 1 / quantile(qiimeNumberOfReads, 0.75))
    ) %>%
    rename(standardId = summaryFieldName) %>%
    mutate(rank = taxonomyRank)

  return(classificationsSummaries)
}

#'
#'
#' @param qiimeClassifications
summarizeQiimeClassifications <- function(qiimeClassifications = NULL) {
  directClassificationsSummaries <- .getDirectQiimeClassificationsSummaries(qiimeClassifications)

  superkingdomClassificationsSummaries <- .getMappeQiimeClassificationsClassificationsSummariesByRank(qiimeClassifications, "superkingdom")
  phylumClassificationsSummaries <- .getMappeQiimeClassificationsClassificationsSummariesByRank(qiimeClassifications, "phylum")
  classClassificationsSummaries <- .getMappeQiimeClassificationsClassificationsSummariesByRank(qiimeClassifications, "class")
  orderClassificationsSummaries <- .getMappeQiimeClassificationsClassificationsSummariesByRank(qiimeClassifications, "order")
  familyClassificationsSummaries <- .getMappeQiimeClassificationsClassificationsSummariesByRank(qiimeClassifications, "family")
  genusClassificationsSummaries <- .getMappeQiimeClassificationsClassificationsSummariesByRank(qiimeClassifications, "genus")
  speciesClassificationsSummaries <- .getMappeQiimeClassificationsClassificationsSummariesByRank(qiimeClassifications, "species")

  mappedClassificationsSummaries <- rbind(
    superkingdomClassificationsSummaries,
    phylumClassificationsSummaries,
    classClassificationsSummaries,
    orderClassificationsSummaries,
    familyClassificationsSummaries,
    genusClassificationsSummaries,
    speciesClassificationsSummaries
  )

  qiimeClassificationsSummaries <- directClassificationsSummaries %>%
    merge(mappedClassificationsSummaries, by = c("rank", "standardId"), all = T)

  # qiimeClassificationsSummaries$directSpingoPercentualOfSequences <-
  #   100 * qiimeClassificationsSummaries$directSpingoNumberOfSequences / nrow(qiimeClassifications)
  # qiimeClassificationsSummaries$mappedSpingoPercentualOfSequences <-
  #   100 * qiimeClassificationsSummaries$mappedSpingoNumberOfSequences / nrow(qiimeClassifications)

  return(qiimeClassificationsSummaries)
}
