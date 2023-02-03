# Landowns

## Version

This is Landowns version 1.0.2.

## Installation

### Dependencies

Before installing Landowns, make sure you have the two dependencies installed, which are the 'terra' and 'FNN' R packages. Landowns should work with version 1.1.3 or more of the FNN package but currently requires version 1.5-21 of terra. To install terra 1.5-21, type the code below into R:

```r
install.packages("https://cran.r-project.org/src/contrib/Archive/terra/terra_1.5-21.tar.gz", repos = NULL, type = "source")
```

You will also need the 'devtools' package and, if on Windows, the [Rtools](https://cran.r-project.org/bin/windows/Rtools/) software to install Landowns from GitHub.

### Install Landowns

Landowns can be installed in R from GitHub using the code below:

```r
library("devtools")
install_github("TamsinWoodman/Landowns")
```

## Using Landowns

Landowns can be loaded in R as follows:

```r
library("Landowns")
```

A tutorial for using Landowns is provided as a vignette with the package. The vignette can be found [here]() or by building and viewing the vignette after installing Landowns.

```r
library("Landowns")
devtools::install(build_vignettes = TRUE)
browseVignettes("Landowns")
```

## News

See the [NEWS](NEWS.md) file for the latest improvements and updates to Landowns.

## Citing Landowns

When using Landowns, please use this citation:

