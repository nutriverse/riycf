# Determining Minimum Acceptable Diet

Identification of individual 6-23 months old children minimum acceptable
diet based on their breastfeeding status

Identification of individual 6-23 months old children dietary practices
meet the minimum requirement of dietary diversity (at least 5 food group
out of 8 food groups)

## Usage

``` r
get_mad(age, q4, mdd, mmf, mmff)

get_mdd(food_score, age)
```

## Arguments

- age:

  This parameter holds the information about child age in the month
  format.

- q4:

  The binary variable which mentions that the child was receiving
  breastfeeding in the previous day (yes = "1", no = "0").

- mdd:

  The binary variable which holds whether the child received the minimum
  dietary diversity or not (yes = "1", no = "0").

- mmf:

  The binary variable which indicates that the child got minimum meal
  frequency ("1") or not ("0").

- mmff:

  This parameter is about the non-breastfed children received minimum
  milk feeding frequency or not (yes = "1", no = "0").

- food_score:

  the integer variable with continuous scale which indicates the number
  of food groups the child consumed in previous day

## Value

binary variable indicate child get the minimum acceptable diet (mad =1 )
or not (mad = 0)

binary variables indicate child get the minimum dietary diversity (mdd =
1 ) or not (mdd = 0)

## Author

Nicholus Tint Zaw

## Examples

``` r
  df <- iycfData
  df$mdd <- rbinom(n = nrow(df), size = 1, prob = 0.5)
  df$mmf <- rbinom(n = nrow(df), size = 1, prob = 0.5)
  df$mmff <- rbinom(n = nrow(df), size = 1, prob = 0.5)

  mad <- get_mad(df$calc_age_months, df$child_bfyest, df$mdd, df$mmf, df$mmff)



# Minimum Dietary Diversity
  df <- iycfData

  df$food_score <- round(runif(nrow(df), min = 0, max = 8), 0)

  mdd <- get_mdd(df$food_score, df$calc_age_months)

```
