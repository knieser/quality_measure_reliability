# Analyzing health care quality measures

QualityMeasure is an R package to help with analyzing health care quality measures. The package includes functions for calculating measure performance, both unadjusted and adjusted, and estimating reliability with several different methods. More detail on each reliability estimation method can be found in [Comparing methods for assessing the reliability of health care quality measures](https://pubmed.ncbi.nlm.nih.gov/39145538/).

<b> This package is a work-in-progress. Please email me at nieser\@stanford.edu with any questions if you plan to use this code. </b>

The following code can be used to download the latest version of the package to your RStudio from Github.

``` r
library(devtools)
devtools::install_github('knieser/quality_measure_reliability')
library(QualityMeasure)
```

## Tutorial

This package contains a vignette describing how this package can be used to estimate reliability: [Example analysis](https://knieser.github.io/QualityMeasure/Example-analysis.html)
