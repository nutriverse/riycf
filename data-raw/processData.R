
# SAMPLE DATASET for IYCF INDICATORS EXAMPLE

################################################################################

# Breastfeeding Sample Data

bfData <- read.csv("data-raw/bf_data.csv")
# usethis::use_data(bfData, overwrite = TRUE, compress = "xz")

################################################################################

# Complementary Feeding Sample Data

cfData <- read.csv("data-raw/iycf_data.csv")
# usethis::use_data(cfData, overwrite = TRUE, compress = "xz")

# IYCF Dataset #
iycfData <- rbind(bfData, cfData)
usethis::use_data(iycfData, overwrite = TRUE, compress = "xz")


################################################################################
