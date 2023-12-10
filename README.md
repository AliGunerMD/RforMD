
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RforMD <a href="https://aligunermd.github.io/RforMD/"><img src="man/figures/logo.png" align="right" height="120" /></a>

<!-- badges: start -->
<!-- badges: end -->

`{RforMD}` aims to provide some functions for personal use.  
Surely, any academic in medicine, can use any.  
I am open to improve. Do not hesitate for requests/issue reports.

## Installation

You can install the development version of RforMD from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("AliGunerMD/RforMD")
library("RforMD")
```

## Example use

``` r
# remotes::install_github("AliGunerMD/RforMD", force = TRUE)
library(RforMD)

library(palmerpenguins)
library(finalfit)
```

The idea of this package comes from the personal needs.  
The [finalfit](https://finalfit.org) package is one of the most useful
package for a researcher who works in medicine.  
It provides great functionalities. I just want to improve some parts of
them, so, try to extent it to save some time.

Later, I also added some functions for
[flextable](https://ardata-fr.github.io/flextable-book/) with the same
idea. to make faster and totally reproducible Word tables.  
If you are happy with html output, try [gt](https://gt.rstudio.com/),
[gtsummary](https://www.danieldsjoberg.com/gtsummary/),
[gtExtras](https://jthomasmock.github.io/gtExtras/) packages.

<br> All analysis starts with the exploration of data.  
Among many options like `summary()`, `str()`, `dplyr::glimpse()`,
`skimr::skim()`;  
I prefer `finalfit::ff_glimpse()`.

with some modifications

``` r
ag_ff_glimpse(penguins, type = "cont")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r
ag_ff_glimpse(penguins, type = "cat") %>% 
        flextable::height(height = 1.5, part = "body") %>% 
        flextable::hrule(rule = "atleast", part = "body")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

``` r
ag_ff_glimpse(penguins, type = "cat", strata = "sex", missing = TRUE, flex_font_size = 9)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />
