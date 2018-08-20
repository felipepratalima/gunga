#' ---
#' title: "gunga"
#' output: github_document
#' ---
knitr::opts_chunk$set(echo = TRUE)

#'
#' **gunga** is a R package which ... .
#' It was developed using the S4 class system, by Felipe Prata Lima (http://lbi.usp.br/membros/) and João Carlos Setubal (http://www.iq.usp.br/setubal/).
#'

#' ## **taxdump** files download
#'
#' The **taxdump** files can be download from the NCBI's ftp site, at ftp://ftp.ncbi.nih.gov/pub/taxonomy/.
#' You can download these files using **wget** with the command:
#'
#' wget ftp://ftp.ncbi.nih.gov/pub/taxonomy/taxdump.tar.gz
#'
#' And uncompress it using **tar**:
#'
#' tar zxvf taxdump.tar.gz
#'
#' This way you should obtain the following files:
#' ```{bash}
#' ls ~/taxdump/
#' ```
#'
#' In this docs, we are going to suppose that you downloaded and uncompressed this in your home folder.

#' ## Install
#'
#' Install the package using devtools:
#'
#' devtools::install_github("felipepratalima/gunga")

#' ## Instantiate the **Taxdumpr** base class
#'
#' Load the package:
# require(taxdumpr)

#'
#' Packages methods are organized around the **Taxdumpr** object. This can be instatiated by the **Taxdumpr** constructor, which requires:
#' 1. nodesDmpLocation: the path to the **nodes.dmp** file from **taxdump** downloaded files.
#' 2. namesDmpLocation: the same to **names.dmp**.
# taxdumprObject <- Taxdumpr(nodesDmpLocation = "~/taxdump/nodes.dmp", namesDmpLocation = "~/taxdump/names.dmp")

#' ## Usage Examples
#'
#' [Centrifuge (Shotgun) + Spingo + QIIME](./CentrifugeShSpingoQiime.md)
#'
