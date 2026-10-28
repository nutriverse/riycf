# Construct exclusive breastfeeding status for under 6 months old child

Identification of individual 0-5 months old children exclusive
breastfeeding status

## Usage

``` r
get_ebf(q4, age, liquid_food, solid_food)
```

## Arguments

- q4:

  The binary variable which mentioned that the child was receiving
  breastfeeding in the previous day (yes = "1", no = "0").

- age:

  This parameter holds the information about child age in the month
  format.

- liquid_food:

  The binary variable which mentioned that the child was receiving any
  type of liquid foods beside breastfeeding yesterday (yes = 1 or no =
  0).

- solid_food:

  The binary variable which mentioned that the child was receiving any
  type of solid foods yesterday (yes = 1 or no = 0).

## Value

A vector of binary values indicating whether a child was exclusively
breastfed or not during the previous day (1 = Yes or 0 = No).

## Author

Nicholus Tint Zaw

## Examples

``` r

 df <- iycfData

# Liquid consumption previous day - yes/no
 liquid <- list(df$child_vitdrop, df$child_ors, df$child_water,
                df$child_juice, df$child_broth, df$child_porridge,
                df$child_bms, df$child_milk, df$child_mproduct,
                df$child_liquid)
#> Warning: Unknown or uninitialised column: `child_vitdrop`.
#> Warning: Unknown or uninitialised column: `child_ors`.
#> Warning: Unknown or uninitialised column: `child_porridge`.
#> Warning: Unknown or uninitialised column: `child_liquid`.

 df$liquid_food <- get_dummy(var_list = liquid)


# Solid food consumption previous day - yes/no
 solid <- list(df$child_rice, df$child_potatoes, df$child_pumpkin,
               df$child_beans, df$child_leafyveg, df$child_mango,
               df$child_fruit, df$child_organ, df$child_beef, df$child_fish,
               df$child_insects, df$child_eggs, df$child_yogurt,
               df$child_fat, df$child_plam, df$child_sweets,
               df$child_condiments)
#> Warning: Unknown or uninitialised column: `child_insects`.
#> Warning: Unknown or uninitialised column: `child_fat`.
#> Warning: Unknown or uninitialised column: `child_plam`.
#> Warning: Unknown or uninitialised column: `child_condiments`.

 df$solid_food <- get_dummy(var_list = solid)


# Child exclusively breastfed
ebf <- get_ebf(df$child_bfyest, df$calc_age_months, df$liquid_food,
               df$solid_food)

```
