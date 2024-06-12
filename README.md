# geneLenDataBase

<!-- badges: start -->
[![R-CMD-check](https://github.com/federicomarini/geneLenDataBase/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/federicomarini/geneLenDataBase/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->
  
The `geneLenDataBase` package provides information on the length of mRNA transcripts 
for a number of genomes and gene ID formats, largely based on UCSC table browser.  
Data objects are provided as individual pieces of information to be retrieved 
and loaded.  
A variety of different gene identifiers and genomes is supported to ensure wide 
applicability.

`geneLenDataBase` can be found on Bioconductor
(<https://www.bioconductor.org/packages/geneLenDataBase>).

## Installation

You can install the version of `geneLenDataBase` which is on Bioconductor with these commands:

``` r
if (!require("BiocManager")) {
  install.packages("BiocManager")
}
BiocManager::install("geneLenDataBase")
```

Alternatively, you can install the development version of `geneLenDataBase` from GitHub with:

``` r
library("remotes")
remotes::install_github("federicomarini/geneLenDataBase")
```

