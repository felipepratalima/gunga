loadRawMajorityVoteProfile <- function(profileLocation = NULL) {
  COLUMNS_TYPES <- readr::cols(
    readr::col_character(),
    readr::col_integer(),
    readr::col_integer(),
    readr::col_integer(),
    readr::col_integer(),
    readr::col_integer(),
    readr::col_integer(),
    readr::col_logical(),
    readr::col_character())
  profileDf <- readr::read_tsv(profileLocation, col_types = COLUMNS_TYPES)
}
