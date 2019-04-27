require(readr)

## Qiime Classifications
# loadRawQiimeClassifications <- function(qiimeClassificationsLocation = NA, method = "uclust") {
#   qiimeClassifications <- readr::read_tsv(qiimeClassificationsLocation, col_names = F)
#
#   if (method == "uclust") {
#     names(qiimeClassifications) <- c("qiimeOtuId", "qiimeLineage", "qiimeScore", "qiimeNumberOfReads")
#   }
#
#   if (method == "rdp") {
#     names(qiimeClassifications) <- c("qiimeOtuId", "qiimeLineage", "qiimeScore")
#   }
#
#   return(qiimeClassifications)
# }

loadRawQiimeClassifications <- function (qiimeClassificationsLocation = NA, method = "uclust") {
  qiimeClassifications <- readr::read_tsv(qiimeClassificationsLocation, col_names = F)
  if (method == "uclust") {
    names(qiimeClassifications) <- c("qiimeOtuId", "qiimeLineage", "qiimeScore", "qiimeNumberOfReads")
  }
  if (method == "rdp") {
    names(qiimeClassifications) <- c("qiimeOtuId", "qiimeLineage", "qiimeScore")
    qiimeClassifications$qiimeNumberOfReads <- 0
  }
  return(qiimeClassifications)
}
