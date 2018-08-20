require(taxdumpr)
require(magrittr)
require(stringr)
require(dplyr)

mergeProfiles <- function(centrifugeGungaProfileForSh = NULL, spingoGungaProfileDf = NULL, qiimeGungaProfileDf = NULL, taxdumprObject = NULL) {
  integratedProfile <- centrifugeGungaProfileForSh %>% merge(spingoGungaProfileDf, by = "speciesId", all = T)

  integratedProfileTaxonomy <- getStandardLineageIdsAndScientificNamesByIdsAsDataFrame(taxdumprObject, unique(integratedProfile$speciesId))
  integratedProfileTaxonomy <- integratedProfileTaxonomy %>% select(speciesId, genusId, phylumId)
  integratedProfile <- merge(integratedProfile, integratedProfileTaxonomy)

  integratedProfile <- merge(integratedProfile, qiimeGungaProfileDf, by = c("genusId"), all.x = T)

  ##
  integratedProfile$dnaCentrifugePercentual <- ifelse(integratedProfile$dnaCentrifugePercentual %>% is.na, 0, integratedProfile$dnaCentrifugePercentual)
  # integratedProfile$rnaCentrifugePercentual <- ifelse(integratedProfile$rnaCentrifugePercentual %>% is.na, 0, integratedProfile$rnaCentrifugePercentual)
  integratedProfile$dnaCentrifugeScoreMedian <- ifelse(integratedProfile$dnaCentrifugeScoreMedian %>% is.na, 0, integratedProfile$dnaCentrifugeScoreMedian)
  # integratedProfile$rnaCentrifugeScoreMedian <- ifelse(integratedProfile$rnaCentrifugeScoreMedian %>% is.na, 0, integratedProfile$rnaCentrifugeScoreMedian)
  integratedProfile$dnaCentrifugeScoreCI <- ifelse(integratedProfile$dnaCentrifugeScoreCI %>% is.na, 0, integratedProfile$dnaCentrifugeScoreCI)
  # integratedProfile$rnaCentrifugeScoreCI <- ifelse(integratedProfile$rnaCentrifugeScoreCI %>% is.na, 0, integratedProfile$rnaCentrifugeScoreCI)
  integratedProfile$centrifugeSh <- ifelse(integratedProfile$centrifugeSh %>% is.na, F, integratedProfile$centrifugeSh)
  # integratedProfile$centrifugeMt <- ifelse(integratedProfile$centrifugeMt %>% is.na, F, integratedProfile$centrifugeMt)

  integratedProfile$spingoPercentual <- ifelse(integratedProfile$spingoPercentual %>% is.na, 0, integratedProfile$spingoPercentual)
  integratedProfile$spingoScoreMedian <- ifelse(integratedProfile$spingoScoreMedian %>% is.na, 0, integratedProfile$spingoScoreMedian)
  integratedProfile$spingoScoreCI <- ifelse(integratedProfile$spingoScoreCI %>% is.na, 0, integratedProfile$spingoScoreCI)
  integratedProfile$spingoAmp <- ifelse(integratedProfile$spingoAmp %>% is.na, F, integratedProfile$spingoAmp)

  integratedProfile$qiimePercentual <- ifelse(integratedProfile$qiimePercentual %>% is.na, 0, integratedProfile$qiimePercentual)
  integratedProfile$qiimeOtusNumber <- ifelse(integratedProfile$qiimeOtusNumber %>% is.na, 0, integratedProfile$qiimeOtusNumber)
  integratedProfile$qiimeAmp <- ifelse(integratedProfile$qiimeAmp %>% is.na, F, integratedProfile$qiimeAmp)

  ## Predicted Species
  integratedProfile$numberOfClassifiedSpeciesByAll <- nrow(integratedProfile) %>% log
  integratedProfile$numberOfClassifiedSpeciesByCentrifugeSh <- nrow(centrifugeGungaProfileForSh) %>% log
  integratedProfile$numberOfClassifiedSpeciesBySpingo <- nrow(spingoGungaProfileDf) %>% log
  integratedProfile$numberOfClassifiedOtusByQiime <- sum(qiimeGungaProfileDf$qiimeOtusNumber) %>% log
  integratedProfile$numberOfClassifiedGenusByQiime <- nrow(qiimeGungaProfileDf) %>% log ## 16S

  return(integratedProfile)
}

# integratedProfile <- mergeProfiles(centrifugeGungaProfileForSh, spingoGungaProfileDf, qiimeGungaProfileDf, taxdumprObject)
# table(duplicated(integratedProfile$speciesId))
