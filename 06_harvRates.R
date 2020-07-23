#### HARV RECLASS AND RATES ####

##########################
#### PRELIM DATA PREP ####
##########################

## Load data
data <- read.csv(paste0(data.dir, "harv_summ_cntyXownXtype.csv")) # skip first row of long names
colnames(data)

## Gather so harv type becomes variable
data <- data %>%
  pivot_longer(-c(1:4,17,18),
               names_to = "harv.type",
               values_to = "plot.cnt")

## Add in reclass of county, for type, harv type, owner
lu.fips <- read.csv(paste0(data.dir, "lu_fips.csv"))
lu.harv <- read.csv(paste0(data.dir, "lu_harv.csv"))
lu.for <- read.csv(paste0(data.dir, "lu_for.csv")) %>% dplyr::select(-for.type.name)

data <- data %>%
  left_join(lu.fips, by = "fips") %>%
  left_join(lu.harv, by = "harv.type") %>%
  left_join(lu.for, by = "for.type")

data <- data %>%
  mutate(own.class = ifelse(owner == 10, "public", # FS
                            ifelse(owner == 20, "public", # other fed
                                   ifelse(owner == 30, "public", # state or local gvt
                                          "private")))) # all private


## TO DOS

# For counties with high ratio of corp priv moreso than fam for, will assign higher stand areas.
# To establish ratio threshold, look at known high corp counties (e.g., Aroostook ME)
# Bin counties into more corp or less corp. > 25%? Add this as column variable.

# combine harv types into resid BA: NO, LOW, MORE, ALL.
# (Check to see if harv types are roughly 1/2 sml first, 1/2 lrg first -- then can be implemented randomly. Else hold that thought...)
# Compute annual prop of given county, own, for type (each 1 record) that receives each harv type (incl. end yrs).
# Bin into quantiles: 0, 0-25, 25-50, 50-75, 75-100 (will be way lower) for each resid BA class.
# For each for type and own type AND corp ratio (more or less), if quantiles match, combine into single MU.
# Now, will be left with some MUs that are wholly within 1 county. Others will cross.
# Assign avg stand size. prob mostly by own (pub, priv corp, priv fam) -- Jacob will do.

# Run resid BA #s by Tony.

# Rasterize all these MUs; remove area > 3000 ft. Send to Jacob.


