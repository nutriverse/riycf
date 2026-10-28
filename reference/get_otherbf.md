# Create other breastfeeding-related indicators

Identification of individual 0-23 months old children breastfeeding
status:

- Ever breastfed;

- Early initiation of breastfeeding;

- Exclusive breastfeeding for the first two days after birth;

- Mixed milk feeding under 6 months;

- Continuous breastfeeding 12-23 months; and,

- Bottle feeding 0-23 months.

## Usage

``` r
get_evbf(q4, age)

get_eibf(age, q2, q2_hour)

get_ebf2d(q3, age)

get_mixmf(q4, age, q6b, q6c)

get_cbf(q4, age)

get_bof(q5, age)
```

## Arguments

- q4:

  An integer vector indicating whether the child breastfed in the
  previous day. 1 = Yes; 0 = No.

- age:

  An integer vector of child's age in months.

- q2:

  An integer vector indicating whether the child was put to the breast
  *immediately*, within *hours*, or within *days*. 0 = immediately; 1 =
  for hours; 2 = for days.

- q2_hour:

  An integer vector for the number of hours after birth the child was
  put to the breast.

- q3:

  An integer vector indicating whether the child had anything else
  beside breastmilk within the first two days after birth. 1 = Yes; 0 =
  No.

- q6b:

  An integer vector indicating whether the child had infant formula
  feeding in the previous day. 1 = Yes; 0 = No.

- q6c:

  An integer vector indicating whether the child had any dairy products
  such as milk from animals or fresh, tinned or powdered milk. 1 = Yes;
  0 = No.

- q5:

  An integer vector indicating whether the child fed with a bottle in
  the previous day. 1 = Yes; 0 = No.

## Value

An integer vector indicating whether the child meets the definition of
the respective breastfeeding indicator: 1 = Yes; 0 = No.

## Author

Nicholus Tint Zaw

## Examples

``` r
df <- iycfData

# Ever Breastfed
evbf <- get_evbf(df$child_bf, df$calc_age_months)

# Early Initiation of Breastfeeding
eibf <- get_eibf(df$calc_age_months, df$child_eibf, df$child_eibf_hrs)

# Exclusive Breastfeeding for the first two days after birth
q3 <- rbinom(n = nrow(df), size = 1, prob = 0.5)

ebf2d <- get_ebf2d(q3, df$calc_age_months)

# Mixed Milk Feeding Under 6 months
mixmf <- get_mixmf(
  df$child_bfyest, df$calc_age_months, df$child_bms, df$child_milk
)

# Continuous Breastfeeding 12-23 months
cbf <- get_cbf(df$child_bfyest, df$calc_age_months)

# Bottle Feeding 0-23 months
q5 <- rbinom(n = nrow(df), size = 1, prob = 0.5)

bof <- get_bof(q5, df$calc_age_months)
```
