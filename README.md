
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
library(tidyverse)

strata <- "species"
table_vars_penguins <- penguins %>% 
        select(-species) %>% 
        names()


# library(finalfit)
# library(flextable)
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
I prefer `finalfit::ff_glimpse()` with some modifications.

### ag_ff_glimpse()

#### Continuous variables

``` r
ag_ff_glimpse(penguins, type = "cont")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

#### Categorical variables

``` r
ag_ff_glimpse(penguins, type = "cat") 
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="80%" style="display: block; margin: auto;" />

#### Categorical variables with strata

``` r
ag_ff_glimpse(penguins, type = "cat", strata = "sex", missing = TRUE) 
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="80%" style="display: block; margin: auto;" />

### ag_shapiro()

``` r
ag_shapiro(penguins, strata = strata, table_vars = table_vars_penguins)
#> Shapiro-Wilk test results for normality within strata:
#> Checked variables: bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g, year
#> Stratified by: species
#> Non-normally distributed variables: body_mass_g, year, bill_length_mm, bill_depth_mm, flipper_length_mm
#> [1] "body_mass_g"       "year"              "bill_length_mm"   
#> [4] "bill_depth_mm"     "flipper_length_mm"
```

``` r
ag_shapiro_results(penguins, strata = strata, table_vars = table_vars_penguins,
                   scientific = TRUE) %>% 
        ag_flex()
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="80%" style="display: block; margin: auto;" />

### ag_qq_plots()

``` r
ag_qq_plots(penguins, strata = strata, table_vars = table_vars_penguins)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

### ag_ff_summary()

``` r
ag_ff_summary(penguins, strata = strata, table_vars = table_vars_penguins) %>% 
        ag_flex()
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="80%" style="display: block; margin: auto;" />

### ag_ff_relocate()

``` r
ag_ff_summary(penguins, strata = strata, table_vars = table_vars_penguins) %>% 
        ag_ff_relocate(order = "TGP") %>% 
        ag_flex()
#> Relocated columns:label -- levels -- Total -- Adelie -- Chinstrap -- Gentoo -- p
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="80%" style="display: block; margin: auto;" />

### ag_ff_columns()

``` r
ag_ff_summary(penguins, strata = strata, table_vars = table_vars_penguins) %>% 
        ag_ff_relocate(order = "TGP") %>% 
        ag_ff_columns(levels = TRUE) %>% 
        ag_flex()
#> Relocated columns:label -- levels -- Total -- Adelie -- Chinstrap -- Gentoo -- p
#> Manual check may be needed for some levels.
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="80%" style="display: block; margin: auto;" />

### ag_ff_labels()

``` r
penguins_names <- c(
        "island" = "Island",
        "bill_length_mm" = "Bill length (mm)",
        "bill_depth_mm" = "Bill depth (mm)",
        "flipper_length_mm" = "Flipper length (mm)",
        "body_mass_g" = "Body mass (g)",
        "sex" = "Sex",
        "year" = "Year"
)

ag_ff_summary(penguins, strata = strata, table_vars = table_vars_penguins) %>% 
        ag_ff_relocate(order = "TGP") %>% 
        ag_ff_columns(levels = TRUE) %>% 
        ag_ff_labels(use_vector = TRUE, vector_name = penguins_names) %>% 
        ag_flex()
#> Relocated columns:label -- levels -- Total -- Adelie -- Chinstrap -- Gentoo -- p
#> Manual check may be needed for some levels.
#> A vector for variable names was used to rename labels.
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="80%" style="display: block; margin: auto;" />

``` r
summary_flextable <- ag_ff_summary(penguins, strata = strata, table_vars = table_vars_penguins) %>% 
        ag_ff_relocate(order = "TGP") %>% 
        ag_ff_columns(levels = TRUE) %>% 
        ag_ff_labels(use_vector = TRUE, vector_name = penguins_names) %>% 
        ag_flex()
#> Relocated columns:label -- levels -- Total -- Adelie -- Chinstrap -- Gentoo -- p
#> Manual check may be needed for some levels.
#> A vector for variable names was used to rename labels.
```

### ag_flex_header_labels()

``` r
summary_flextable %>% 
        ag_flex_header_labels(.dataset = penguins, strata = "species")
#> Because custom = FALSE, original stratas will be used.
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="80%" style="display: block; margin: auto;" />

### ag_flex_center()

``` r
summary_flextable %>% 
        ag_flex_header_labels(.dataset = penguins, strata = "species") %>% 
        ag_flex_center() 
#> Because custom = FALSE, original stratas will be used.
```

<img src="man/figures/README-unnamed-chunk-15-1.png" width="80%" style="display: block; margin: auto;" />

### ag_flex_title()

``` r
summary_flextable %>% 
        ag_flex_header_labels(.dataset = penguins, strata = "species") %>% 
        ag_flex_center() %>% 
        ag_flex_title(1, "Characteristics of Penguins dataset")
#> Because custom = FALSE, original stratas will be used.
```

<img src="man/figures/README-unnamed-chunk-16-1.png" width="80%" style="display: block; margin: auto;" />

### ag_flex_abbr()

``` r

abbr_vector <- c(
        "mm" = "Milimeter",
        "g" = "Gram"
)

summary_flextable %>% 
        ag_flex_header_labels(.dataset = penguins, strata = "species") %>% 
        ag_flex_center() %>% 
        ag_flex_title(1, "Characteristics of Penguins dataset") %>% 
        ag_flex_abbr(abbr = abbr_vector) 
#> Because custom = FALSE, original stratas will be used.
```

<img src="man/figures/README-unnamed-chunk-17-1.png" width="80%" style="display: block; margin: auto;" />

### ag_flex_footnote()

``` r
my_random_foot <- "Palmerpenguins provided a great dataset for data exploration & visualization, as an alternative to iris."

summary_flextable %>% 
        ag_flex_header_labels(.dataset = penguins, strata = "species") %>% 
        ag_flex_center() %>% 
        ag_flex_title(1, "Characteristics of Penguins dataset") %>% 
        ag_flex_abbr(abbr = abbr_vector) %>% 
        ag_flex_footnote(my_random_foot) 
#> Because custom = FALSE, original stratas will be used.
```

<img src="man/figures/README-unnamed-chunk-18-1.png" width="80%" style="display: block; margin: auto;" />

### ag_flex_hline()

``` r
summary_flextable %>% 
        ag_flex_header_labels(.dataset = penguins, strata = "species") %>% 
        ag_flex_center() %>% 
        ag_flex_title(1, "Characteristics of Penguins dataset") %>% 
        ag_flex_abbr(abbr = abbr_vector) %>% 
        ag_flex_footnote(my_random_foot) %>% 
        ag_flex_hline()
#> Because custom = FALSE, original stratas will be used.
```

<img src="man/figures/README-unnamed-chunk-19-1.png" width="80%" style="display: block; margin: auto;" />

### Any flextable function can be added

``` r
summary_flextable %>% 
        ag_flex_header_labels(.dataset = penguins, strata = "species") %>% 
        ag_flex_center() %>% 
        ag_flex_title(1, "Characteristics of Penguins dataset") %>% 
        ag_flex_abbr(abbr = abbr_vector) %>% 
        ag_flex_footnote(my_random_foot) %>% 
        ag_flex_hline() %>%  
        flextable::fontsize(size = 7, part = "all")
#> Because custom = FALSE, original stratas will be used.
```

<img src="man/figures/README-unnamed-chunk-20-1.png" width="80%" style="display: block; margin: auto;" />

### ag_flex_save(orientation = “Landscape”)

And saved as  
““(my_output_path, paste0(format(Sys.time(),”%Y%m%d\_%H%M”),“*” ,
”Table*”, n , “.docx”)))“”
