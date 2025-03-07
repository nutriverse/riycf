% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_othercf.R
\name{get_isssf}
\alias{get_isssf}
\alias{get_eff}
\alias{get_swb}
\alias{get_ufc}
\alias{get_zvf}
\title{Construct other breastfeeding related indicators (from IYCF)}
\usage{
get_isssf(solid_food, age)

get_eff(egg_meat, age)

get_swb(sweet, age)

get_ufc(unhealthy, age)

get_zvf(vege_fruit, age)
}
\arguments{
\item{solid_food}{The binary variable which mentioned that the child was
received any type of solid foods yesterday (yes = 1 or no = 0).}

\item{age}{This parameter holds the information about child age in the month
format.}

\item{egg_meat}{The binary variable which mentioned that the child was
received any egg and/or flesh food yesterday}

\item{sweet}{The binary variable which mentioned that the child was
received any sweet beverage foods yesterday}

\item{unhealthy}{The binary variable which mentioned that the child was
received the unhealthy food consumption: sweets and instant foods}

\item{vege_fruit}{The binary variable which mentioned that the child was
received any vegetables or fruits yesterday}
}
\value{
binary variables indicate child met the respective complementary
feeding status or not (yes = 1 or no = 0)
}
\description{
Identification of individual 0-23 months old children
breastfeeding status
}
\examples{

  df <- iycfData

# Introduction of Solid, Semi-solid or soft foods (6-8 months)
# Solid food consumption previous day - yes/no
 solid <- list(df$child_rice, df$child_potatoes, df$child_pumpkin,
               df$child_beans, df$child_leafyveg, df$child_mango,
               df$child_fruit, df$child_organ, df$child_beef, df$child_fish,
               df$child_insects, df$child_eggs, df$child_yogurt,
               df$child_fat, df$child_plam, df$child_sweets,
               df$child_condiments)

 df$solid_food <- get_dummy(var_list = solid)

 isssf <- get_isssf(df$solid_food, df$calc_age_months)


# Egg and/or Flesh food consumption (6-23 months)
# Egg and/or Flesh food consumption previous day - yes/no
 egg_meat <- list(df$child_organ, df$child_beef, df$child_fish,
                  df$child_insects, df$child_eggs)

 df$egg_meat <- get_dummy(var_list = egg_meat)

 eff <- get_eff(df$egg_meat, df$calc_age_months)

# Sweet beverage consumption (6-23 months)
# Sweet beverage consumption previous day - yes/no
 sweet <- list(df$child_milk_sweet, df$child_mproduct_sweet,
               df$child_chocolate, df$child_juice, df$child_soda,
               df$child_tea_sweet, df$child_oth_drink_swee)

 df$sweet <- get_dummy(var_list = sweet)

 swb <- get_swb(df$sweet, df$calc_age_months)

# Unhealthy food consumption (6-23 months)
# Unhealthy food consumption previous day - yes/no
 unhealthy <- list(df$child_sweets, df$child_condiments)

 df$unhealthy <- get_dummy(var_list = unhealthy)

 ufc <- get_ufc(df$unhealthy, df$calc_age_months)

# Zero vegetable or fruit consumption (6-23 months)
# Zero vegetable or fruit consumption previous day - yes/no
 vege_fruit <- list(df$child_pumpkin, df$child_leafyveg, df$child_mango,
                    df$child_fruit)

 df$vege_fruit <- get_dummy(var_list = vege_fruit)

 zvf <- get_zvf(df$vege_fruit, df$calc_age_months)

}
\author{
Nicholus Tint Zaw
}
