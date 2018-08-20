require(taxdumpr)
require(magrittr)
require(stringr)
require(dplyr)

#'
#'
#' @param centrifugeClassificationsLocation
loadRawCentrifugeClassifications <- function(centrifugeClassificationsLocation = "") {
  if (centrifugeClassificationsLocation %>% is.null) {
    stop("The centrifugeClassificationsLocation parameter should not be null")
  }

  if (centrifugeClassificationsLocation %>% is.na) {
    stop("The centrifugeClassificationsLocation parameter should not be NA")
  }

  if (centrifugeClassificationsLocation %>% str_trim == "") {
    stop("The centrifugeClassificationsLocation parameter should not be empty")
  }

  if (centrifugeClassificationsLocation %>% file.exists == F) {
    stop("The centrifugeClassificationsLocation parameter should not be an inexistent file")
  }

  centrifugeClassificationsDf <- read.table(file = centrifugeClassificationsLocation,
                                            header = T, sep = "\t",
                                            colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

  return(centrifugeClassificationsDf)
}

# centrifugeClassifications <- loadCentrifugeClassifications("/home/prata/projects/gunga/inst/extdata/sh/centrifuge_classifications.tsv")
# centrifugeClassifications <- loadCentrifugeClassifications("/home/prata/projects/LocalFolder/RProjectMix1Simulation/sh/centrifuge_classifications.tsv")
# str(centrifugeClassifications)
# View(centrifugeClassifications$classificationsSummariesDf)

# numberOfSequences <- centrifugeClassificationsDf %>% nrow
#
# centrifugeClassificationsSummariesDf <- centrifugeClassificationsDf %>%
#   group_by(taxID) %>%
#   summarise(percentual = n() / numberOfSequences * 100,
#             scoreMedian = median(score),
#             scoreCI = qnorm(0.975) * sd(score) / sqrt(n()),
#             hitLengthMedian = median(hitLength),
#             hitLengthCI = qnorm(0.975) * sd(hitLength) / sqrt(n()),
#             queryLengthMedian = median(queryLength),
#             queryLengthCI = qnorm(0.975) * sd(queryLength) / sqrt(n()))
#
# centrifugeClassificationsObject <- list(
#   numberOfSequences = numberOfSequences,
#   numberOfClassifiedSequencesForSpecies = numberOfSequences,
#   numberOfUnclassifiedSequences = numberOfSequences,
#   rawClassificationsDf = centrifugeClassificationsDf,
#   classificationsDf = centrifugeClassificationsDf,
#   classificationsSummariesDf = centrifugeClassificationsSummariesDf
# )
