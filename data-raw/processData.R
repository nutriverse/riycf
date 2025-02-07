# SAMPLE DATASET for IYCF INDICATORS EXAMPLE

## Breastfeeding Sample Data
bfData <- read.csv("data-raw/bf_data.csv")

# usethis::use_data(bfData, overwrite = TRUE, compress = "xz")


## Complementary Feeding Sample Data
cfData <- read.csv("data-raw/iycf_data.csv")
# usethis::use_data(cfData, overwrite = TRUE, compress = "xz")


## IYCF Dataset - CARE Myanmar Sample data
iycfData <- rbind(bfData, cfData)

## required additional variable creation - to matched with WHO questionnaires
iycfData$bf_2days <- rbinom(n = nrow(iycfData), size = 1, prob = 0.3)
iycfData$bf_bottle <- rbinom(n = nrow(iycfData), size = 1, prob = 0.15)
iycfData$child_milk_sweet <- rbinom(n = nrow(iycfData), size = 1, prob = 0.3)
iycfData$child_milk_sweet <- ifelse(iycfData$child_milk == 0, NA,
                                    iycfData$child_milk)
iycfData$child_mproduct_sweet <- rbinom(n = nrow(iycfData), size = 1,
                                        prob = 0.3)
iycfData$child_mproduct_sweet <- ifelse(iycfData$child_mproduct == 0, NA,
                                        iycfData$child_mproduct)
iycfData$child_chocolate <- rbinom(n = nrow(iycfData), size = 1, prob = 0.3)
iycfData$child_soda <- rbinom(n = nrow(iycfData), size = 1, prob = 0.3)
iycfData$child_tea <- iycfData$child_liquid
iycfData$child_tea_sweet <- rbinom(n = nrow(iycfData), size = 1, prob = 0.3)
iycfData$child_tea_sweet <- ifelse(iycfData$child_tea == 0, NA,
                                   iycfData$child_tea)
iycfData$child_oth_drink <- rbinom(n = nrow(iycfData), size = 1, prob = 0.3)
iycfData$child_oth_drink_sweet <- rbinom(n = nrow(iycfData), size = 1,
                                         prob = 0.3)
iycfData$child_oth_drink_sweet <- ifelse(iycfData$child_oth_drink == 0, NA,
                                         iycfData$child_oth_drink)
iycfData$child_yogurt_frq <- ifelse(iycfData$child_yogurt == 0, NA, 1)
iycfData$child_oth_veg <- rbinom(n = nrow(iycfData), size = 1, prob = 0.3)
iycfData$child_processmeat <- iycfData$child_insects
iycfData$child_cheese <- iycfData$child_fat
iycfData$child_snack <- iycfData$child_condiments
iycfData$child_oth_food <- rbinom(n = nrow(iycfData), size = 1, prob = 0.3)

final_var <- c('csex', 'calc_age_months', 'child_bf', 'child_eibf',
               'child_eibf_hrs', 'child_eibf_days', 'bf_2days', 'child_bfyest',
               'bf_bottle', 'child_water', 'child_bms', 'child_bms_freq',
               'child_milk', 'child_milk_freq', 'child_milk_sweet',
               'child_mproduct', 'child_mproduct_freq', 'child_mproduct_sweet',
               'child_chocolate', 'child_juice', 'child_soda', 'child_tea',
               'child_tea_sweet', 'child_broth', 'child_oth_drink',
               'child_oth_drink_sweet', 'child_yogurt', 'child_yogurt_frq',
               'child_rice', 'child_pumpkin', 'child_potatoes',
               'child_leafyveg', 'child_oth_veg', 'child_mango', 'child_fruit',
               'child_organ', 'child_processmeat', 'child_beef', 'child_eggs',
               'child_fish', 'child_beans', 'child_cheese', 'child_sweets',
               'child_snack', 'child_oth_food', 'child_food_freq')

iycfData <- tibble::as_tibble(iycfData[, final_var])

usethis::use_data(iycfData, overwrite = TRUE, compress = "xz")
