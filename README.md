
<!-- README.md is generated from README.Rmd. Please edit that file -->

# riycf: Utilities for Calculating Infant and Young Child Feeding Indicators <img src="man/figures/logo.png" width="200px" align="right" />

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/nutriverse/riycf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nutriverse/riycf/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/nutriverse/riycf/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/nutriverse/riycf/actions/workflows/test-coverage.yaml)
[![Codecov test
coverage](https://codecov.io/gh/nutriverse/riycf/branch/main/graph/badge.svg)](https://app.codecov.io/gh/nutriverse/riycf?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/nutriverse/riycf/badge)](https://www.codefactor.io/repository/github/nutriverse/riycf)
[![DOI](https://zenodo.org/badge/410309510.svg)](https://zenodo.org/badge/latestdoi/410309510)
<!-- badges: end -->

## What are the IYCF Indicators?

The first 1,000 days of life (from pregnancy to a child’s 2nd birthday)
are critical for addressing childhood malnutrition, especially stunting.
Infant and young child feeding practices (IYCF) largely overlap with
this period as they cover breastfeeding and complementary feeding
practices for the first two years of a child’s life. They also have a
significant impact on childhood health, nutrition outcomes, and child
survival. It is, therefore, critical for countries to measure IYCF
practices as part of their efforts to monitor their progress toward
Sustainable Development Goal 2. It is also important for development
agencies to be able to monitor and evaluate their programs aimed at
improving infant and young child feeding practices towards improved
overall childhood nutrition.

WHO and UNICEF released the first IYCF indicators definition and
measurement guidelines in 2008. In 2021, these guidelines were updated
along with a revised standard questionnaire to capture the information
required to calculate the updated IYCF indicators since the first
initial publication. In general, the IYCF indicators can be categorized
into three main categories: *(1) breastfeeding indicators*, *(2)
complementary indicators*, and *(3) other indicators*.

## Why the {riycf} package?

Although the initial measurement guidelines were published in 2008 with
many humanitarian organisations and country health ministries
implementing these indicators, no comprehensive statistical programming
package to calculate these indicators is yet available. Therefore, every
time the researchers (or humanitarian organisations) are required to
collect and analyse, they need a considerable amount of time to recode
all the syntax (depending on which statistical program they are using).
That is time-consuming work.

This `{riycf}` package aims to address that technical gap by providing
an easy-to-use package of functions to calculate all IYCF indicators
provided in the [WHO
guidelines](https://www.who.int/publications/i/item/9789240018389). This
include comprehensive guidelines for step-by-step usage of each
automated function to analyse individual IYCF indicators using R.

## Installation

The `{riycf}` package is not yet available on
[CRAN](https://cran.r-project.org) but can be installed via the
[nutriverse R Universe](https://nutriverse.r-universe.dev):

``` r
install.packages(
  "riycf",
  repos = c('https://nutriverse.r-universe.dev', 'https://cloud.r-project.org')
)
```

Current development version of `{riycf}` can also be installed using the
`{pak}` package as follows:

``` r
if (require("pak")) install.packages("pak")
pak::pak("nutriverse/riycf")
```

## How does the package work?

Based on the WHO guideline’s indicator definition, the `{riycf}` package
functions will calculate all the IYCF indicators. Each `{riycf}`
function will perform the following tasks.

### (Beneficial in) Data cleaning

Each IYCF package function will perform the data quality check to ensure
all the required data (variables) were correctly constructed in the
dataset. For example, the minimum meal frequency indicator analysis
requires the following variables for data analysis: child age,
breastfeeding status, and frequency of child feeding on the previous
day. The child age and child meal frequency data should be present in
the `numeric - integer` format, and the breastfeeding status should be
coded as a binary true/false variable with yes = 1 and no = 0. The
`{riycf}` package function will ensure that integer variables are
actually integers and variables that need to be recoded into numeric
scores are recoded accordingly.

### IYCF indicator generation

The indicator calculation process would continue if there were no issues
with the data entry. The new IYCF indicator variables will be generated
based on which indicators the user asks to calculate. For example,
suppose the user wanted to calculate whether the beneficiaries meet the
minimum meal frequency. In this case, the user can use the `get_mmf()`
functions to create a new dichotomous variable that indicates whether
each child met the minimum meal frequency. More sample codes from this
package were present in each function documentation.

## Data collection with computer-assisted personal interviews (CAPI)

This package also provides the already programmed IYCF Questionnaires
(based on WHO sample IYCF questionnaires) in XLS programming format.
Detailed guidelines for accessing those forms are provided in the
vignette article called “WHO IYCF Questionnaire XLS Forms.” The
different types of XLS programmed IYCF questionnaires can download on
[this GitHub page](https://github.com/nicholustintzaw/iycf_xls_forms).

## Citation

If you use `{riycf}` in your work, please cite using the suggested
citation provided by a call to the `citation` function as follows:

``` r
citation("riycf")
#> Warning in citation("riycf"): could not determine year for 'riycf' from package
#> DESCRIPTION file
#> To cite package 'riycf' in publications use:
#> 
#>   Tint Zaw N, Guevarra E (????). _riycf: Utilities for Calculating
#>   Infant and Young Child Feeding Indicators_. R package version
#>   0.0.0.9000, https://github.com/nutriverse/riycf,
#>   <https://nutriverse.io/riycf/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {riycf: Utilities for Calculating Infant and Young Child Feeding Indicators},
#>     author = {Nicholus {Tint Zaw} and Ernest Guevarra},
#>     note = {R package version 0.0.0.9000, https://github.com/nutriverse/riycf},
#>     url = {https://nutriverse.io/riycf/},
#>   }
```

## Community guidelines

Feedback, bug reports, and feature requests are welcome; file issues or
seek support [here](https://github.com/nutriverse/riycf/issues). If you
would like to contribute to the package, please see our [contributing
guidelines](https://nutriverse.io/riycf/CONTRIBUTING.html).

This project is released with a [Contributor Code of
Conduct](https://nutriverse.io/riycf/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
