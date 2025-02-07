#'
#' Infant and young child feeding indicators sample dataset
#'
#' This is an example IYCF dataset with 359 observations and 46
#' variables. The dataset is from CARE International Myanmar country programme.
#' All personally identifiable information have been excluded.
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
#' *bf_bottle* | Q5 | Has child drank anything from a bottle with a nipple yesterday? 1 = Yes; 0 = No.
#' *child_water* | Q6A | Has child drank *plain water* yesterday? 1 = Yes; 0 = No.
#' *child_bms* | Q6B | Has child drank *infant formula* yesterday? 1 = Yes; 0 = No.
#' *child_bms_freq* | Q6Bnum | How many times has child drank *infant formula*? Integer value.
#' *child_milk* | Q6C | Has child drank *milk from animals* yesterday? 1 = Yes; 0 = No.
#' *child_milk_freq* | Q6Cnum | How many times has child drank *milk from animals*? Integer value.
#' *child_milk_sweet* | Q6Cswt | For children who drank *milk from animals*, was milk sweetened or flavoured? 1 = Yes; = No.
#' *child_mproduct* | Q6D | Has child drank *yogurt drinks* yesterday? 1 = Yes; 0 = No.
#' *child_mproduct_freq* | Q6Dnum | How many times has child rank *yogurt drinks*? Integer value.
#' *child_mproduct_sweet* | Q6Dswt | Yogurt drinks - sweet or flavored type yogurt
#' *child_chocolate* | Q6E | Chocolate-flavored drinks
#' *child_juice* | Q6F| Fruit juice or fruit-flavored drinks
#' *child_soda* | Q6G | Sodas, malt drinks, sports drinks or energy drinks
#' *child_tea* | Q6H | Tea, coffee, or herbal drinks
#' *child_tea_sweet* | Q6Hswt | Tea, coffee, or herbal drinks - sweetened
#' *child_broth* | Q6I | Clear broth or clear soup
#' *child_oth_drink* | Q6J | Any other liquids
#' *child_oth_drink_sweet* | Q6Jswt | Other liquids - sweetened
#' *child_yogurt* | Q7A | Yogurt, other than yogurt drinks
#' *child_yogurt_frq* | Q7Anum | Yogurt, other than yogurt drinks - frequency
#' *child_rice* | Q7B | Porridge, bread, rice, noodles, pasta
#' *child_pumpkin* | Q7C | Pumpkin, carrots, sweet potatoes (yellow or orange inside)
#' *child_potatoes* | Q7D | Plantains, white potatoes, white yams, manioc, cassava
#' *child_leafyveg* | Q7E | Dark green leafy vegetables
#' *child_oth_veg* | Q7F | Any other vegetables
#' *child_mango* | Q7G | Ripe mangoes, ripe papayas
#' *child_fruit* | Q7H | Any other fruits
#' *child_organ* | Q7I | Liver, kidney, heart
#' *child_processmeat* | Q7J | Sausages, hot dogs, ham, bacon, salami, canned meat
#' *child_beef* | Q7K | Any other meat, such as beef, pork, lamb, goat, chicken, duck
#' *child_eggs* | Q7L | Eggs
#' *child_fish* | Q7M | Fresh fish, dried fish or shellfish
#' *child_beans* | Q7N | Beans, peas, lentils, nuts , seeds
#' *child_cheese* | Q7O | Hard or soft cheese
#' *child_sweets* | Q7P | Sweet foods such as chocolates, candies, cakes and biscuits
#' *child_snack* | Q7Q | Chips, crisps, puffs, French fries, instant noodles
#' *child_oth_food* | Q7R |Any other solid, semi-solid or soft food
#' *child_food_freq* | Q8 | number of any solid, semi-solid or soft foods yesterday
#'
#' @source CARE Myanmar
#'
#' @examples
#' iycfData
#'

"iycfData"


