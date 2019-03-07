loadRawQiimeProfile <- function(qiimeProfileLocation = NA) {
  qiimeProfile <- readr::read_tsv(qiimeProfileLocation, skip = 1)

  names(qiimeProfile) <- c("qiimeLineage", "qiimePercentageOfReads")
  qiimeProfile$qiimeLineage <- qiimeProfile$qiimeLineage %>% as.character

  return(qiimeProfile)
}
