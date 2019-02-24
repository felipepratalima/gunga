require(magrittr)
require(taxdumpr)

#'
#'
#' @param taxonomyIds
#' @param taxdumprObject
getStandardIdsDataFrame <- function(taxonomyIds = NULL, taxdumprObject = NULL) {
  taxonomyIds <- unique(taxonomyIds)
  standardIds <- taxonomyIds %>% getStandardTaxonomyIdsByIds(taxdumprObject, .)
  ranks <- standardIds %>% getTaxonomyRanksByIds(taxdumprObject, .)
  standardDf <- data.frame(taxonomyId = taxonomyIds, standardId = standardIds, rank = ranks)
  return(standardDf)
}
