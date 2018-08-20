gunga
================
prata
Fri Aug 17 17:08:15 2018

``` r
knitr::opts_chunk$set(echo = TRUE)
```

**gunga** is a R package which ... . It was developed using the S4 class system, by Felipe Prata Lima (<http://lbi.usp.br/membros/>) and João Carlos Setubal (<http://www.iq.usp.br/setubal/>).

**taxdump** files download
--------------------------

The **taxdump** files can be download from the NCBI's ftp site, at <ftp://ftp.ncbi.nih.gov/pub/taxonomy/>. You can download these files using **wget** with the command:

wget <ftp://ftp.ncbi.nih.gov/pub/taxonomy/taxdump.tar.gz>

And uncompress it using **tar**:

tar zxvf taxdump.tar.gz

This way you should obtain the following files:

``` bash
ls ~/taxdump/
```

    ## citations.dmp
    ## delnodes.dmp
    ## division.dmp
    ## gc.prt
    ## gencode.dmp
    ## merged.dmp
    ## names.dmp
    ## nodes.dmp
    ## readme.txt
    ## taxdump.tar.gz

In this docs, we are going to suppose that you downloaded and uncompressed this in your home folder. \#\# Install

Install the package using devtools:

devtools::install\_github("felipepratalima/gunga") \#\# Instantiate the **Taxdumpr** base class

Load the package:

``` r
# require(taxdumpr)
```

Packages methods are organized around the **Taxdumpr** object. This can be instatiated by the **Taxdumpr** constructor, which requires: 1. nodesDmpLocation: the path to the **nodes.dmp** file from **taxdump** downloaded files. 2. namesDmpLocation: the same to **names.dmp**.

``` r
# taxdumprObject <- Taxdumpr(nodesDmpLocation = "~/taxdump/nodes.dmp", namesDmpLocation = "~/taxdump/names.dmp")
```

Usage Examples
--------------

[Centrifuge (Shotgun) + Spingo + QIIME](./CentrifugeShSpingoQiime.md)
