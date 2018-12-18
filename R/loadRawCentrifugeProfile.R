require(taxdumpr)
require(magrittr)
require(stringr)
require(dplyr)
require(readr)

#'
#'
#' @param centrifugeProfileLocation
loadRawCentrifugeProfile <- function(centrifugeProfileLocation = "") {
  if (centrifugeProfileLocation %>% is.null) {
    stop("The centrifugeProfileLocation parameter should not be null")
  }

  if (centrifugeProfileLocation %>% is.na) {
    stop("The centrifugeProfileLocation parameter should not be NA")
  }

  if (centrifugeProfileLocation %>% str_trim == "") {
    stop("The centrifugeProfileLocation parameter should not be empty")
  }

  if (centrifugeProfileLocation %>% file.exists == F) {
    stop("The centrifugeProfileLocation parameter should not be an inexistent file")
  }

  # centrifugeProfileDf <- read.table(file = centrifugeProfileLocation,
  #                                           header = T, sep = "\t",
  #                                           colClasses = c("character", "numeric", "character", "numeric", "numeric", "numeric", "numeric"))
  centrifugeProfileDf <- readr::read_tsv(file = centrifugeProfileLocation, col_types = c(col_character(), col_integer(), col_character(), col_double(), col_integer(), col_integer(), col_double()))

  return(centrifugeProfileDf)
}

# centrifugeProfile <- loadCentrifugeProfile(CENTRIFUGE_PROFILE_LOCATION)
