# LandScaleR

## Version

This is **LandScaleR** version 1.0.2.

## Installation

### Dependencies

Before installing **LandScaleR** make sure you have the two dependencies 
installed, which are the 'terra' and 'FNN' R packages. **LandScaleR** should
work with version 1.1.3 or more of the FNN package but currently requires 
version 1.5-21 of terra. To install terra 1.5-21, type the code below into R:

```r
install.packages("https://cran.r-project.org/src/contrib/Archive/terra/terra_1.5-21.tar.gz", repos = NULL, type = "source")
```

You will also need the 'devtools' package and, if on Windows, the 
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) software to install 
**LandScaleR** from GitHub.

### Install LandScaleR

**LandScaleR** can be installed in R from GitHub using the code below:

```r
library("devtools")
install_github("TamsinWoodman/LandScaleR", ref = "optimiseRcode", build_vignettes = TRUE)
```

## Using LandScaleR

**LandScaleR** can be loaded in R as follows:

```r
library("LandScaleR")
```

A tutorial for using **LandScaleR** is provided as a vignette with the 
package. The vignette can be viewed after installing 
**LandScaleR**:

```r
library("LandScaleR")
browseVignettes("LandScaleR")
```

## News

See the [NEWS](NEWS.md) file for the latest improvements and updates to 
**LandScaleR**.
