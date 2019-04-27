require(magrittr)
require(dplyr)

# .getDirectQiimeClassificationsSummaries <- function(qiimeClassifications = NULL) {
#   directClassificationsSummaries <- qiimeClassifications %>%
#     subset(standardId %>% is.na == F) %>%
#     group_by(rank, standardId) %>%
#     summarize(directQiimeNumberOfSequences = n(),
#               directQiimeScoreMean = mean(qiimeScore),
#               directQiimeScoreSD = sd(qiimeScore),
#               directQiimeScoreCI = qnorm(0.975) * sd(qiimeScore) / sqrt(n()),
#               directQiimeScoreMedian = median(qiimeScore),
#               directQiimeScoreQ1 = quantile(qiimeScore, 0.25),
#               directQiimeScoreQ3 = quantile(qiimeScore, 0.75),
#               directQiimeScoreIQR = IQR(qiimeScore),
#               directQiimeScoreMAD = mad(qiimeScore),
#               directQiimeScoreNonNormalMAD = mad(qiimeScore, constant = 1 / quantile(qiimeScore, 0.75)),
#               directQiimeNumberOfSequencesMean = mean(qiimeNumberOfReads),
#               directQiimeNumberOfSequencesSD = sd(qiimeNumberOfReads),
#               directQiimeNumberOfSequencesCI = qnorm(0.975) * sd(qiimeNumberOfReads) / sqrt(n()),
#               directQiimeNumberOfSequencesMedian = median(qiimeNumberOfReads),
#               directQiimeNumberOfSequencesQ1 = quantile(qiimeNumberOfReads, 0.25),
#               directQiimeNumberOfSequencesQ3 = quantile(qiimeNumberOfReads, 0.75),
#               directQiimeNumberOfSequencesIQR = IQR(qiimeNumberOfReads),
#               directQiimeNumberOfSequencesMAD = mad(qiimeNumberOfReads),
#               directQiimeNumberOfSequencesNonNormalMAD = mad(qiimeNumberOfReads, constant = 1 / quantile(qiimeNumberOfReads, 0.75))
#     )
#
#   ## convert to char, beacuse sometimes we have non recognized taxonomies
#   directClassificationsSummaries$standardId <- directClassificationsSummaries$standardId %>% as.character
#
#   return(directClassificationsSummaries)
# }
#
# .getMappeQiimeClassificationsSummariesByRank <- function(qiimeClassifications = NULL, taxonomyRank = "species") {
#   summaryFieldName <- taxonomyRank %>% paste0("Id")
#   summaryIndexes <- qiimeClassifications[[summaryFieldName]] %>% is.na == F
#   classificationsSummaries <-
#     qiimeClassifications %>%
#     subset(summaryIndexes) %>%
#     group_by_(summaryFieldName) %>%
#     summarize(mappedQiimeNumberOfSequences = n(),
#               mappedQiimeScoreMean = mean(qiimeScore),
#               mappedQiimeScoreSD = sd(qiimeScore),
#               mappedQiimeScoreCI = qnorm(0.975) * sd(qiimeScore) / sqrt(n()),
#               mappedQiimeScoreMedian = median(qiimeScore),
#               mappedQiimeScoreQ1 = quantile(qiimeScore, 0.25),
#               mappedQiimeScoreQ3 = quantile(qiimeScore, 0.75),
#               mappedQiimeScoreIQR = IQR(qiimeScore),
#               mappedQiimeScoreMAD = mad(qiimeScore),
#               mappedQiimeScoreNonNormalMAD = mad(qiimeScore, constant = 1 / quantile(qiimeScore, 0.75)),
#               mappedQiimeNumberOfSequencesMean = mean(qiimeNumberOfReads),
#               mappedQiimeNumberOfSequencesSD = sd(qiimeNumberOfReads),
#               mappedQiimeNumberOfSequencesCI = qnorm(0.975) * sd(qiimeNumberOfReads) / sqrt(n()),
#               mappedQiimeNumberOfSequencesMedian = median(qiimeNumberOfReads),
#               mappedQiimeNumberOfSequencesQ1 = quantile(qiimeNumberOfReads, 0.25),
#               mappedQiimeNumberOfSequencesQ3 = quantile(qiimeNumberOfReads, 0.75),
#               mappedQiimeNumberOfSequencesIQR = IQR(qiimeNumberOfReads),
#               mappedQiimeNumberOfSequencesMAD = mad(qiimeNumberOfReads),
#               mappedQiimeNumberOfSequencesNonNormalMAD = mad(qiimeNumberOfReads, constant = 1 / quantile(qiimeNumberOfReads, 0.75))
#     ) %>%
#     rename(standardId = summaryFieldName) %>%
#     mutate(rank = taxonomyRank)
#
#   return(classificationsSummaries)
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
              directQiimeNumberOfSequencesNonNormalMAD = mad(qiimeNumberOfReads, constant = 1 / quantile(qiimeNumberOfReads, 0.75)),
              directQiimeOtuNumberOfSequencesMean = mean(qiimeOtuNumberOfReads),
              directQiimeOtuNumberOfSequencesSD = sd(qiimeOtuNumberOfReads),
              directQiimeOtuNumberOfSequencesCI = qnorm(0.975) * sd(qiimeOtuNumberOfReads) / sqrt(n()),
              directQiimeOtuNumberOfSequencesMedian = median(qiimeOtuNumberOfReads),
              directQiimeOtuNumberOfSequencesQ1 = quantile(qiimeOtuNumberOfReads, 0.25),
              directQiimeOtuNumberOfSequencesQ3 = quantile(qiimeOtuNumberOfReads, 0.75),
              directQiimeOtuNumberOfSequencesIQR = IQR(qiimeOtuNumberOfReads),
              directQiimeOtuNumberOfSequencesMAD = mad(qiimeOtuNumberOfReads),
              directQiimeOtuNumberOfSequencesNonNormalMAD = mad(qiimeOtuNumberOfReads, constant = 1 / quantile(qiimeOtuNumberOfReads, 0.75))
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
              mappedQiimeNumberOfSequencesNonNormalMAD = mad(qiimeNumberOfReads, constant = 1 / quantile(qiimeNumberOfReads, 0.75)),
              mappedQiimeOtuNumberOfSequencesMean = mean(qiimeOtuNumberOfReads),
              mappedQiimeOtuNumberOfSequencesSD = sd(qiimeOtuNumberOfReads),
              mappedQiimeOtuNumberOfSequencesCI = qnorm(0.975) * sd(qiimeOtuNumberOfReads) / sqrt(n()),
              mappedQiimeOtuNumberOfSequencesMedian = median(qiimeOtuNumberOfReads),
              mappedQiimeOtuNumberOfSequencesQ1 = quantile(qiimeOtuNumberOfReads, 0.25),
              mappedQiimeOtuNumberOfSequencesQ3 = quantile(qiimeOtuNumberOfReads, 0.75),
              mappedQiimeOtuNumberOfSequencesIQR = IQR(qiimeOtuNumberOfReads),
              mappedQiimeOtuNumberOfSequencesMAD = mad(qiimeOtuNumberOfReads),
              mappedQiimeOtuNumberOfSequencesNonNormalMAD = mad(qiimeOtuNumberOfReads, constant = 1 / quantile(qiimeOtuNumberOfReads, 0.75))
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

  superkingdomClassificationsSummaries <- .getMappeQiimeClassificationsSummariesByRank(qiimeClassifications, "superkingdom")
  phylumClassificationsSummaries <- .getMappeQiimeClassificationsSummariesByRank(qiimeClassifications, "phylum")
  classClassificationsSummaries <- .getMappeQiimeClassificationsSummariesByRank(qiimeClassifications, "class")
  orderClassificationsSummaries <- .getMappeQiimeClassificationsSummariesByRank(qiimeClassifications, "order")
  familyClassificationsSummaries <- .getMappeQiimeClassificationsSummariesByRank(qiimeClassifications, "family")
  genusClassificationsSummaries <- .getMappeQiimeClassificationsSummariesByRank(qiimeClassifications, "genus")
  speciesClassificationsSummaries <- .getMappeQiimeClassificationsSummariesByRank(qiimeClassifications, "species")

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
