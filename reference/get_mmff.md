# Determining Minimum Milk Feeding Frequency

Identification of individual 6-23 months old children minimum milk
feeding frequency for non-breastfeed children 6-23 months (MMFF)

## Usage

``` r
get_mmff(q4, age, milk_frq)

get_milk_frq(q6bnum, q6cnum, q6dnum, q7anum)
```

## Arguments

- q4:

  The binary variable which mentioned that the child was receiving
  breastfeeding in the previous day (yes = "1", no = "0").

- age:

  This parameter holds the information about child age in the month
  format.

- milk_frq:

  the integer variable with continuous scale represents all the milk
  consumption frequency (beside breastmilk)

- q6bnum:

  This parameter indicates the number of infant formula feeding the
  children received in previous days.

- q6cnum:

  This parameter holds information about the frequency of milk related
  food feeding the child received yesterday (milk from animals, such as
  fresh, tinned, or powdered milk)

- q6dnum:

  This parameter presents the information about the frequency of yogurt
  drinks the child received in the previous day.

- q7anum:

  the integer variable with continuous scale represents the frequency of
  yogurt (solid or semi-solid) the child consumed yesterday.

## Value

The output of the `get_mmff` and `get_milk_frq` variables includes the
following added variable.

|  |  |
|----|----|
| **Variables** | **Description** |
| *mf_frq* | continuous variables indicate non-breastfeed child consume number of milk and milk-product food feeding |
| *mmff* | binary variables indicate child get the minimum milk feeding frequency (mmff =1 ) or not (mmff = 0) |

## Author

Nicholus Tint Zaw

## Examples

``` r

# Calculate Non-breastfeed Children Milk Frequency
 df <- iycfData

 df$milk_frq <- get_milk_frq(df$child_bms_freq, df$child_milk_freq,
                             df$child_mproduct_freq, df$child_yogurt)

# MMFF
df$mmff <- get_mmff(df$child_bfyest, df$calc_age_months, df$milk_frq)

```
