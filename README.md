
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nbtrd

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/CSAFE-ISU/nbtrd.svg?branch=master)](https://travis-ci.org/CSAFE-ISU/nbtrd)
[![Coverage
status](https://codecov.io/gh/CSAFE-ISU/nbtrd/branch/master/graph/badge.svg)](https://codecov.io/github/CSAFE-ISU/nbtrd?branch=master)

The goal of nbtrd is to provide an automated interface to the NBTRD for
essential tasks, such as downloading sample x3p data (for `x3ptools` and
`bulletxtrctr`) or uploading new scans of x3p data to NBTRD.

## Installation

You can install the released version of nbtrd from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("nbtrd")
```

## Sample Scripts

`inst/UploadSet.R` contains a script that (if filled in correctly) will
upload your data set of x3p files to NBTRD.
