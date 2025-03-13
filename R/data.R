#'
#' Infant and young child feeding indicators sample dataset
#'
#' This is an example IYCF dataset from CARE International Myanmar country
#' programme. All personally identifiable information have been excluded.
#'
#' @format A data frame with 46 columns and 359 rows.
#'
#' **Variables** | **WHO Question Number** | **Description**
#' :--- | :--- | :---
#' *csex* | | Sex of child; 1 = male; 0 = female.
#' *calc_age_months* | | Age of child in months (calculated).
#' *child_bf* | Q1 | Has child ever been breastfed?; 1 = Yes; 0 = No.
#' *child_eibf* | Q2 | When was child first put to the breast: 0 = immediately; 1 = hours; 2 = days; 999 = Don't know/no response.
#' *child_eibf_hrs* | Q2 | If *child_eibf* is 1, number of hours child first put to the breast; integer value.
#' *child_eibf_days* | Q2 | If *child_eibf* is 2, number of days child first put to the breast; integer value.
#' *bf_2days* | Q3 | Has child been given anything other than breast milk within the child's first 2 days of age? 1 = Yes; 0 = No.
#' *child_bfyest* | Q4 | Has child been breastfed yesterday during the day or at night? 1 = Yes; 0 = No.
#' *bf_bottle* | Q5 | Has child had anything from a bottle with a nipple yesterday? 1 = Yes; 0 = No.
#' *child_water* | Q6A | Has child had *plain water* yesterday? 1 = Yes; 0 = No.
#' *child_bms* | Q6B | Has child had *infant formula* yesterday? 1 = Yes; 0 = No.
#' *child_bms_freq* | Q6Bnum | How many times has child had *infant formula*? Integer value.
#' *child_milk* | Q6C | Has child had *milk from animals* yesterday? 1 = Yes; 0 = No.
#' *child_milk_freq* | Q6Cnum | How many times has child had *milk from animals*? Integer value.
#' *child_milk_sweet* | Q6Cswt | For children who had *milk from animals*, was milk sweetened or flavoured? 1 = Yes; = No.
#' *child_mproduct* | Q6D | Has child had *yogurt drinks* yesterday? 1 = Yes; 0 = No.
#' *child_mproduct_freq* | Q6Dnum | How many times has child had *yogurt drinks*? Integer value.
#' *child_mproduct_sweet* | Q6Dswt | Has child had *sweet or flavoured yogurt drinks* yesterday? 1 = Yes; 0 = No.
#' *child_chocolate* | Q6E | Has child had *chocolate-flavoured drinks* yesterday? 1 = Yes; 0 = No.
#' *child_juice* | Q6F| Has child had *fruit juice or fruit-flavoured drinks* yesterday? 1 = Yes; 0 = No.
#' *child_soda* | Q6G | Has child had *sodas, malt drinks, sports drinks, or energy drinks* yesterday? 1 = Yes; 0 = No.
#' *child_tea* | Q6H | Has child had *tea, coffee, or herbal drinks* yesterday? 1 = Yes; 0 = No.
#' *child_tea_sweet* | Q6Hswt | Has child had *sweetened tea, coffee, or herbal drinks* yesterday? 1 = Yes; 0 = No.
#' *child_broth* | Q6I | Has child had *clear broth or clear soup* yesterday? 1 = Yes; 0 = No.
#' *child_oth_drink* | Q6J | Has child had *any other liquids* yesterday? 1 = Yes; 0 = No.
#' *child_oth_drink_sweet* | Q6Jswt | Has child had *any other sweetened liquids* yesterday? 1 = Yes; 0 = No.
#' *child_yogurt* | Q7A | Has child had *yogurt other than yogurt drinks* yesterday? 1 = Yes; 0 = No.
#' *child_yogurt_frq* | Q7Anum | How many times has child had *yogurt other than yogurt drinks* yesterday? 1 = Yes; 0 = No.
#' *child_rice* | Q7B | Has child had *porridge, bread, rice, noodles, pasta* yesterday? 1 = Yes; 0 = No.
#' *child_pumpkin* | Q7C | Has child had *pumpkin, carrots, sweet potatoes (yellow or orange inside)* yesterday? 1 = Yes; 0 = No.
#' *child_potatoes* | Q7D | Has child had *plantains, white potatoes, white yams, manioc, cassava* yesterday? 1 = Yes; 0 = No.
#' *child_leafyveg* | Q7E | Has child had *dark green leafy vegetables* yesterday? 1 = Yes; 0 = No.
#' *child_oth_veg* | Q7F | Has child had *any other vegetables* yesterday? 1 = Yes; 0 = No.
#' *child_mango* | Q7G | Has child had *ripe mangoes, ripe papayas* yesterday? 1 = Yes; 0 = No.
#' *child_fruit* | Q7H | Has child had *any other fruits* yesterday? 1 = Yes; 0 = No.
#' *child_organ* | Q7I | Has child had *liver, kidney, heart* yesterday? 1 = Yes; 0 = No.
#' *child_processmeat* | Q7J | Has child had *sausages, hot dogs, ham, bacon, salami, canned meat* yesterday? 1 = Yes; 0 = No.
#' *child_beef* | Q7K | Has child had *any other meat, such as beef, pork, lamb, goat, chicken, duck* yesterday? 1 = Yes; 0 = No.
#' *child_eggs* | Q7L | Has child had *eggs* yesterday? 1 = Yes; 0 = No.
#' *child_fish* | Q7M | Has child had *fresh fish, dried fish, or shellfish* yesterday? 1 = Yes; 0 = No.
#' *child_beans* | Q7N | Has child had *beans, peas, lentils, nuts, seeds* yesterday? 1 = Yes; 0 = No.
#' *child_cheese* | Q7O | Has child had *hard or soft cheese* yesterday? 1 = Yes; 0 = No.
#' *child_sweets* | Q7P | Has child had *sweet foods such as chocolates, candies, cakes, and biscuits* yesterday? 1 = Yes; 0 = No.
#' *child_snack* | Q7Q | Has child had *chips, crisps, puffs, french fries, instant noodles* yesterday? 1 = Yes; 0 = No.
#' *child_oth_food* | Q7R | Has child had *any other solid, semi-solid or soft food* yesterday? 1 = Yes; 0 = No.
#' *child_food_freq* | Q8 | How many times has child had *any solid, semi-solid, or soft foods* yesterday? 1 = Yes; 0 = No.
#'
#' @source CARE Myanmar
#'
#' @examples
#' iycfData
#'

"iycfData"
