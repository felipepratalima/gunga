require(taxdumpr)
require(magrittr)
require(stringr)
require(dplyr)

makeSpingoProfileForAmplicon <- function(rawSpingoProfileLocation = NULL, rawSpingoClassificationsLocation = NULL, taxdumprObject = NULL) {
  spingoClassifications <- read.delim(rawSpingoClassificationsLocation, header = F, sep = "\t")
  spingoClassificationsSummaries <- spingoClassifications %>%
    group_by(V7) %>%
    summarise(spingoScoreMedian = median(V2),
              spingoScoreCI = qnorm(0.975) * sd(V2) / sqrt(length(V2)))

  spingoProfile <- read.delim(rawSpingoProfileLocation, header = F, sep = "\t")
  spingoProfile <- spingoProfile %>% merge(spingoClassificationsSummaries, by.x = "V1", by.y = "V7")
  spingoProfile$speciesName <- spingoProfile$V1 %>% str_replace_all("_", " ")
  spingoProfile$speciesId <- getStandardTaxonomyIdsByNames(taxdumprObject, spingoProfile$speciesName)
  spingoProfile <- spingoProfile %>% subset(speciesId %>% is.na == F)
  spingoProfile$spingoPercentual <- spingoProfile$V2

  spingoProfileForAmp <- spingoProfile %>% select(speciesId, spingoPercentual, spingoScoreMedian, spingoScoreCI)

  spingoProfileForAmp$spingoAmp <- TRUE

  return(spingoProfileForAmp)
}
