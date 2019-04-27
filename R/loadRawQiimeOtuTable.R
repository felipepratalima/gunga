require(readr)

loadRawQiimeOtuTable <- function(qiimeOtuTableLocation = "") {
  otuTable <- readr::read_tsv(qiimeOtuTableLocation, skip = 2,
                              col_names = c("qiimeOtuId", "qiimeOtuNumberOfReads", "qiimeLineage"),
                              col_types = c(col_character(), col_integer(), col_character()))
  return(otuTable)
}
