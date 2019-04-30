getStandardParentIdsAsDataFrame <- function(taxonomyIds = NULL, taxadumprObject = NULL) {
  childIds <- unique(taxonomyIds)
  parentdIds <- sapply(childIds, function(childId) {
    childDf <- taxdumpr::getStandardLineageIdsByIds(taxadumprObject, childId)
    parentIndex <- nrow(childDf) - 1
    childDf$lineageId[parentIndex]
  })
  returnDf <- data.frame(taxonomyId = childIds, parentId = parentdIds)
  returnDf
}
