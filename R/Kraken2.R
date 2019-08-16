loadRawKraken2Classifications <- function(kraken2ClassificationsLocation = "") {
  rawClassifications <-
    readr::read_tsv(kraken2ClassificationsLocation,
                    col_names = c("kraken2Status", "kraken2ReadId", "kraken2TaxonomyId",
                                  "kraken2ReadLength", "kraken2Score", "kraken2Lca"),
                    col_types = c(col_character(), col_character(), col_integer(),
                                  col_integer(), col_character(), col_character()))
  return(rawClassifications)
}

processKraken2Classifications <- function (rawKraken2Classifications = NULL, taxdumprObject = NULL) {
  if (rawKraken2Classifications %>% is.null) {
    stop("The rawKraken2Classifications parameter should not be null")
  }
  
  if (rawKraken2Classifications %>% is.na) {
    stop("The rawKraken2Classifications parameter should not be NA")
  }
  
  if (rawKraken2Classifications %>% nrow == 0) {
    stop("The rawKraken2Classifications parameter should not be empty")
  }
  
  rawKraken2Classifications$taxonomyId <- rawKraken2Classifications$kraken2TaxonomyId
  rawKraken2Classifications$taxonomyId <- rawKraken2Classifications$taxonomyId %>% getUpdatedIds(taxdumprObject, .)
  
  standardDf <- getStandardIdsDataFrame(unique(rawKraken2Classifications$taxonomyId), taxdumprObject)
  rawKraken2Classifications <- rawKraken2Classifications %>% merge(standardDf, by = "taxonomyId")
  
  taxonomyDf <- unique(rawKraken2Classifications$taxonomyId) %>% getStandardLineageIdsAndScientificNamesByIdsAsDataFrame(taxdumprObject, .)
  rawKraken2Classifications <- rawKraken2Classifications %>% merge(taxonomyDf, by = "taxonomyId")
  
  ## Process score
  rawKraken2Classifications$kraken2Score <- rawKraken2Classifications$kraken2Score %>% str_replace("P=", "") %>% as.numeric
  
  return(rawKraken2Classifications)
}

.computeCI <- function(values = NA) {
  n <- length(values)
  ci <- qnorm(0.975) * sd(values) / sqrt(n)
  return(ci)
}

.computeQ1 <- function(value = NA) {
  q1 <- quantile(value, 0.25, na.rm = T)
  return(q1)
}

.computeQ3 <- function(value = NA) {
  q3 <- quantile(value, 0.75, na.rm = T)
  return(q3)
}

.computeNonNormalMAD <- function(value) {
  nnmad <- mad(value, constant = 1 / quantile(value, 0.75, na.rm = T))
  return(nnmad)
}

.getDirectKraken2ClassificationsSummaries <- function(kraken2Classifications = NULL) {
  directClassificationsSummaries <- kraken2Classifications %>%
    subset(standardId %>% is.na == F) %>%
    group_by(rank, standardId) %>%
    summarize(directKraken2NumberOfSequences = n())
  
  kraken2ScoreSummaries <- kraken2Classifications %>%
    subset(standardId %>% is.na == F) %>%
    group_by(rank, standardId) %>%
    summarize_at(.vars = vars("kraken2ReadLength", "kraken2Score"),
                 .funs = funs(sd, CI = .computeCI, median, Q1 = .computeQ1, Q3 = .computeQ3, IQR(., na.rm = T), mad, NNMAD = .computeNonNormalMAD))
  colnamesIndexes <- colnames(kraken2ScoreSummaries) %in% c("rank", "standardId") == F
  colnames(kraken2ScoreSummaries)[colnamesIndexes] <- paste0("direct_", colnames(kraken2ScoreSummaries)[colnamesIndexes])
  
  directClassificationsSummaries <- directClassificationsSummaries %>% merge(kraken2ScoreSummaries, by = c("rank", "standardId"))
  
  ## convert to char, beacuse sometimes we have non recognized taxonomies
  directClassificationsSummaries$standardId <- directClassificationsSummaries$standardId %>% as.character
  
  return(directClassificationsSummaries)
}

.getMappedKraken2ClassificationsSummariesByRank <- function(kraken2Classifications = NULL, taxonomyRank = "species") {
  summaryFieldName <- taxonomyRank %>% paste0("Id")
  summaryIndexes <- kraken2Classifications[[summaryFieldName]] %>% is.na == F
  classificationsSummaries <-
    kraken2Classifications %>%
    subset(summaryIndexes) %>%
    group_by_(summaryFieldName) %>%
    summarize(mappedKrakenNumberOfSequences = n()) %>%
    rename(standardId = summaryFieldName) %>%
    mutate(rank = taxonomyRank)
  
  kraken2ScoreSummaries <- kraken2Classifications %>%
    subset(summaryIndexes) %>%
    group_by_(summaryFieldName) %>%
    summarize_at(.vars = vars("kraken2ReadLength", "kraken2Score"),
                 .funs = funs(sd, CI = .computeCI, median, Q1 = .computeQ1, Q3 = .computeQ3, IQR(., na.rm = T), mad, NNMAD = .computeNonNormalMAD)) %>%
    rename(standardId = summaryFieldName) %>%
    mutate(rank = taxonomyRank)
  colnamesIndexes <- colnames(kraken2ScoreSummaries) %in% c("rank", "standardId") == F
  colnames(kraken2ScoreSummaries)[colnamesIndexes] <- paste0("mapped_", colnames(kraken2ScoreSummaries)[colnamesIndexes])
  
  classificationsSummaries <- classificationsSummaries %>% merge(kraken2ScoreSummaries, by = c("rank", "standardId"))
  
  return(classificationsSummaries)
}

summarizeKraken2Classifications <- function(kraken2Classifications = NULL) {
  directClassificationsSummaries <- .getDirectKraken2ClassificationsSummaries(kraken2Classifications)
  
  superkingdomClassificationsSummaries <- .getMappedKraken2ClassificationsSummariesByRank(kraken2Classifications, "superkingdom")
  phylumClassificationsSummaries <- .getMappedKraken2ClassificationsSummariesByRank(kraken2Classifications, "phylum")
  classClassificationsSummaries <- .getMappedKraken2ClassificationsSummariesByRank(kraken2Classifications, "class")
  orderClassificationsSummaries <- .getMappedKraken2ClassificationsSummariesByRank(kraken2Classifications, "order")
  familyClassificationsSummaries <- .getMappedKraken2ClassificationsSummariesByRank(kraken2Classifications, "family")
  genusClassificationsSummaries <- .getMappedKraken2ClassificationsSummariesByRank(kraken2Classifications, "genus")
  speciesClassificationsSummaries <- .getMappedKraken2ClassificationsSummariesByRank(kraken2Classifications, "species")
  
  mappedClassificationsSummaries <- rbind(
    superkingdomClassificationsSummaries,
    phylumClassificationsSummaries,
    classClassificationsSummaries,
    orderClassificationsSummaries,
    familyClassificationsSummaries,
    genusClassificationsSummaries,
    speciesClassificationsSummaries
  )
  
  kraken2ClassificationsSummaries <- directClassificationsSummaries %>%
    merge(mappedClassificationsSummaries, by = c("rank", "standardId"), all = T)
  
  # kraken2ClassificationsSummaries$directSpingoPercentualOfSequences <-
  #   100 * kraken2ClassificationsSummaries$directSpingoNumberOfSequences / nrow(kraken2Classifications)
  # kraken2ClassificationsSummaries$mappedSpingoPercentualOfSequences <-
  #   100 * kraken2ClassificationsSummaries$mappedSpingoNumberOfSequences / nrow(kraken2Classifications)
  
  return(kraken2ClassificationsSummaries)
}

loadRawKraken2Profile <- function (kraken2ProfileLocation = "") {
  if (kraken2ProfileLocation %>% is.null) {
    stop("The kraken2ProfileLocation parameter should not be null")
  }
  if (kraken2ProfileLocation %>% is.na) {
    stop("The kraken2ProfileLocation parameter should not be NA")
  }
  if (kraken2ProfileLocation %>% str_trim == "") {
    stop("The kraken2ProfileLocation parameter should not be empty")
  }
  if (kraken2ProfileLocation %>% file.exists == F) {
    stop("The kraken2ProfileLocation parameter should not be an inexistent file")
  }
  kraken2Profile <-
    readr::read_tsv(file = kraken2ProfileLocation,
                    col_names = c("kraken2PercentageOfReads", "kraken2CladeNumberOfReads", "kraken2TaxonNumberOfReads",
                                  "kraken2Rank", "kraken2TaxonomyId", "kraken2TaxonomyName"),
                    col_types = c(col_double(), col_integer(), col_integer(),
                                  col_character(), col_integer(), col_character()))
  return(kraken2Profile)
}

processKraken2Profile <- function (rawKraken2Profile = NULL, taxdumprObject = NULL) {
  if (rawKraken2Profile %>% is.null) {
    stop("The rawKraken2Profile parameter should not be null")
  }
  if (rawKraken2Profile %>% is.na) {
    stop("The rawKraken2Profile parameter should not be NA")
  }
  if (rawKraken2Profile %>% nrow == 0) {
    stop("The rawKraken2Profile parameter should not be empty")
  }
  
  rawKraken2Profile$kraken2PercentageOfReads <- rawKraken2Profile$kraken2PercentageOfReads %>% str_replace("%", "") %>% as.numeric
  
  rawKraken2Profile$taxonomyId <- rawKraken2Profile$kraken2TaxonomyId
  rawKraken2Profile$taxonomyId <- rawKraken2Profile$taxonomyId %>%getUpdatedIds(taxdumprObject, .)
  
  standardDf <- getStandardIdsDataFrame(unique(rawKraken2Profile$taxonomyId), taxdumprObject)
  rawKraken2Profile <- rawKraken2Profile %>% merge(standardDf, by = "taxonomyId")
  
  taxonomyDf <- unique(rawKraken2Profile$taxonomyId) %>% getStandardLineageIdsAndScientificNamesByIdsAsDataFrame(taxdumprObject, .)
  rawKraken2Profile <- rawKraken2Profile %>% merge(taxonomyDf, by = "taxonomyId")
  
  percentageOfClassifiedReads <- rawKraken2Profile %>% subset(kraken2Rank == "D") %>% summarize(classifiedPercentual = sum(kraken2PercentageOfReads)) %>% .$classifiedPercentual
  rawKraken2Profile <- rawKraken2Profile %>% subset(kraken2Rank %in% c("D", "P", "C", "O", "F", "G", "S"))
  rawKrakenShProfileSummaries <- rawKraken2Profile %>% 
    group_by(rank, standardId) %>% summarize(kraken2PercentageOfReadsSum = sum(kraken2PercentageOfReads),
                                             kraken2CladeNumberOfReadsSum = sum(kraken2CladeNumberOfReads),
                                             kraken2TaxonNumberOfReadsSum = sum(kraken2TaxonNumberOfReads)) %>%
    mutate(kraken2PercentageOfClassifiedReadsSum = kraken2PercentageOfReadsSum / percentageOfClassifiedReads)
  
  return(rawKrakenShProfileSummaries)
}

assignKraken2ProfileAsMetatranscriptomics <- function (kraken2Profile = NULL, kraken2ClassificationsSummaries = NULL) {
  if (kraken2Profile %>% is.null) {
    stop("The kraken2Profile parameter should not be null")
  }
  
  if (kraken2Profile %>% is.na) {
    stop("The kraken2Profile parameter should not be NA")
  }
  
  if (kraken2ClassificationsSummaries %>% is.null) {
    stop("The kraken2ClassificationsSummaries parameter should not be null")
  }
  
  if (kraken2ClassificationsSummaries %>% is.na) {
    stop("The kraken2ClassificationsSummaries parameter should not be NA")
  }
  
  gungaProfile <- kraken2Profile %>% merge(kraken2ClassificationsSummaries, by = c("rank", "standardId"))
  
  namesIndexes <- (names(gungaProfile) %in% c("rank", "standardId") == FALSE)
  names(gungaProfile)[namesIndexes] <- names(gungaProfile)[namesIndexes] %>% paste0("ForMt")
  
  gungaProfile$kraken2Mt <- TRUE
  
  return(gungaProfile)
}
