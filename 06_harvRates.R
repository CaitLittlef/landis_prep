#### HARV RECLASS AND RATES ####

## Load data
data <- read.csv(paste0(data.dir, "harv_summ_cntyXownXtype.csv")) # skip first row of long names
colnames(data)
data <- data %>% rename(ttl.plots.cnty.own.for = ttl.plots)

orig <- data


## Gather so harv type becomes variable
data <- data %>%
  pivot_longer(-c(1:4,17,18), # exclude these columns
               names_to = "harv.type",
               values_to = "plot.cnt")



## Load look-ups
lu.fips <- read.csv(paste0(data.dir, "lu_fips.csv"))
lu.harv <- read.csv(paste0(data.dir, "lu_harv.csv")) %>% dplyr::select(-order.first)
lu.for <- read.csv(paste0(data.dir, "lu_for.csv")) %>% dplyr::select(-for.type.name)



## Add county, state, harv reclass and forest type reclass
data <- data %>%
  left_join(lu.fips, by = "fips") %>%
  left_join(lu.harv, by = "harv.type") %>%
  left_join(lu.for, by = "for.type")



## Reclass ownerships
data <- data %>%
  mutate(own.reclass = ifelse(owner == 10, "public", # FS
                            ifelse(owner == 20, "public", # other fed
                                   ifelse(owner == 30, "public", # state or local gvt
                                          "private")))) # all private



## Drop orig for and owner types
data <- data %>% dplyr::select(-owner, -for.type, -harv.type)



## Ttl plots specific to finer res (orig) county x owner x for type but not harv. Drop and use plot.cnts
# plot.cnts are specific to harv type AND county x own x for.
# But because I've re-classed owner, for type, and harv type, now need to combine plot.cnts.
data <- data %>%
  group_by(fips, county, state, yr.first, yr.last, for.reclass, own.reclass, harv.reclass) %>%
  summarise(plot.cnt = sum(plot.cnt)) %>%
  ungroup()



## Create new summary by county, forest, owner (i.e., mgmt unit = MU
data <- data %>%
  group_by(fips, county, state, yr.first, yr.last, for.reclass, own.reclass) %>%
  mutate(plot.ttl.mu = sum(plot.cnt)) %>%
  ungroup()

  

## Create MU-level summaries of prop harv by each type; annual rates; harv return interval
data <- data %>%
  mutate(prop.plots = plot.cnt/plot.ttl.mu) %>%
  mutate(ann.rate = prop.plots / (yr.last - yr.first + 1)) %>% #+1 for yrs inclusive
  mutate(hri = ifelse(ann.rate == 0,NA,(1/ann.rate))) %>% # set NA if there's no harv ever.
  dplyr::select(-yr.first, -yr.last)



## Scope only harv areas
data.harv.only <- data %>% filter(! harv.reclass  == "d.all.resid")

hist(data.harv.only$hri)


## Is it reasonable that we don't paramterize low?
temp <- data %>% filter(for.reclass == "low", ! harv.reclass == "d.all.resid") 
sum(temp$plot.cnt) # only 6 where some harvest took place. ok.








hist(data[!is.na(data$hri),]$hri, nclass = 20)

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

# Some counties may not have FIA data -- at all, or for a given for type or ownership. Select nearest?
