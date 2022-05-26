
<!-- README.md is generated from README.Rmd. Please edit that file -->

# riycf: Utilities for Calculating Infant and Young Child Feeding Indicators <img src="man/figures/logo.png" width="200px" align="right" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/nutriverse/riycf/workflows/R-CMD-check/badge.svg)](https://github.com/nutriverse/riycf/actions)
[![test-coverage](https://github.com/nutriverse/riycf/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/nutriverse/riycf/actions/workflows/test-coverage.yaml)
[![Codecov test
coverage](https://codecov.io/gh/nutriverse/riycf/branch/main/graph/badge.svg)](https://app.codecov.io/gh/nutriverse/riycf?branch=main)
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
into three main categories: (1) breastfeeding indicators, (2)
complementary indicators, and (3) other indicators, which are focused on
bottle-feeding practices and the generation of data visualization plots
for the breastfeeding status by age.

## Why riycf package?

Although the initial measurement guidelines were published in 2008 with
many humanitarian organizations and country health ministries
implementing these indicators, no comprehensive statistical programming
package to calculate these indicators is yet available. Therefore, every
time the researchers (or humanitarian organizations) are required to
collect and analyze, they need a considerable amount of time to re-code
all the syntax (depending on which statistical program they are using).
That is time-consuming work.

This riycf package aims to address that technical gap by providing an
easy-to-use package of automated functions\` to calculate all IYCF
indicators provided in the WHO guideline ([the Indicators for assessing
infant and young child feeding practices: definitions and measurement
methods](https://www.who.int/publications/i/item/9789240018389)). This
include comprehensive guidelines for step-by-step usage of each
automated function to analyze individual IYCF indicators in R software
to make it easier for those less familiar with R software.

## Installation

``` r
if(!require(remotes)) install.packages("remotes") 
remotes::install_github("nutriverse/riycf")
```

## How does the package work?

Based on the WHO guideline’s indicator definition, the riycf package
functions will calculate all the IYCF indicators. Each riycf function
will perform the following tasks.

1.  *(Beneficial in) Data cleaning*: Each IYCF package function will
    perform the data quality check to ensure all the required data
    (variables) were correctly constructed in the dataset. For example,
    the minimum meal frequency indicator analysis requires the following
    variables for data analysis: child age, breastfeeding status, and
    frequency of child feeding on the previous day. The child age and
    child meal frequency data should be present in the
    `numeric - integer` format, and the breastfeeding status should be
    coded as a binary true/false variable with yes = 1 and no = 0. The
    riycf package function will ensure that integer variables are
    actually integers and variables that need to be re-coded into
    numeric scores are re-coded.
2.  *IYCF indicator generation*: The indicator calculation process would
    continue if there were no issues with the data entry. The new IYCF
    indicator variables will be generated based on which indicators the
    user asks to calculate. For example, suppose the user wanted to
    calculate whether the beneficiaries meet the minimum meal frequency.
    In this case, the user can use the `get_mmf` functions to create a
    new dichotomous variable that indicates whether each child met the
    minimum meal frequency. More sample codes from this package were
    present in each function documentation.

## Data Collection with CATI

This package also provides the already programmed IYCF Questionnaires
(based on WHO sample IYCF questionnaires) in XLS programming format.
Detailed guidelines for accessing those forms are provided in the
vignette article called “WHO IYCF Questionnaire XLS Forms.” The
different types of XLS programmed IYCF questionnaires can download on
[this Github page](https://github.com/nicholustintzaw/iycf_xls_forms).

## Code of Conduct

Please note that the riycf project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
