## Adapt BAU parameters to reflect RRT changes.
# Re-allocate all single tree...
# and take 0.2 of annual harv from inci+unharv...
# and put towards RRT tx for 1x; 0.4 for 2x intensity.

############################################################
## Load BAU parameters, remove extraneous first row
bau <- read.csv(paste0(out.dir, "parametersXcountyXforXown_2020-10-22.csv")) %>% dplyr::select(-X)

head(bau, 5)

# Reassociate with regions (aggregated counties) by which I defined THO area. 
counties <- read.csv(paste0(data.dir, "lu_counties.csv"))
# counties$fips <- as.factor(counties$fips)
counties$state <- as.factor(counties$state)
counties$county <- as.factor(counties$county)
counties$area_class <- as.factor(counties$area_class)

# Add area clas name. Then make sure order is 1, 2, 3, 4.
counties <- counties %>%
  mutate(area_class_name = ifelse(area_class == 1, "1sml (eg RI)",
                               ifelse(area_class == 2, "2sml/mid (eg sVT)",
                                      ifelse(area_class == 3, "3mid/lrg (eg nVT)",
                                             "4lrg (eg nME)")))) 
counties$area_class_name <- as.factor(counties$area_class_name)


# Binned as follows:
# ME-n - biggest, class 4
# ME-s - big, class 3
# MA-e - smallest, class 1
# MA-w - small, class 2
# NH-n - biggest, class 4
# NH-s - big, class 3
# NJ - smallest, class 1
# NY-down - smallest, class 1
# NY-up - small, class 2
# PA - big, class 3
# RI - smallest, class 1
# VT-n - big, class 3
# VT-s - small, class 2

bau <- bau %>% left_join(counties[,c(1,4,5)], by = "fips")


############################################################
## Combine inci & uharv

# Spread so each harv.type is displayed as a column.
# But harvest-specific parameters makes this tricky. Remove for now.
foo <- bau %>%
  dplyr::select(-first.year, -resid.ba.ft2ac, -remove.order, -harv.return.int) %>%
  pivot_wider(names_from = harv.type, values_from = ann.rate.avg)

# So adding below works, set all NA to zero.
foo[is.na(foo)] <- 0

# Combine invi & unharv. Unclear if this is problematic b/c of diff start yrs. 
foo <- foo %>%
  group_by(fips, area_class, for.type, own.type) %>% # these are the unique values
  mutate(d.unharv.inci = d.unharv + d.inci) %>% # combine unharv and inci
  dplyr::select(-d.unharv, -d.inci) %>% # remove old unharv and inci
  ungroup()

# Add summary row to ensure they all = 1
foo <- foo %>%
  mutate(e.all.harv = rowSums(.[12:22])) %>%
  ungroup()
# Franklin and Middlesex counties - (Frank MA & NY, Middlesex MA & CT)...
# have slightly off numbers 8 instances total. Nip/tuck from unharv to reconcile.


foo$d.unharv.inci <- ifelse(foo$e.all.harv > 1,
                            foo$d.unharv.inci - (foo$e.all.harv - 1),
                            ifelse(foo$e.all.harv < 1,
                                   foo$d.unharv.inci + (1 - foo$e.all.harv),
                                   foo$d.unharv.inci))
# Corrected?
foo <- foo %>%
  mutate(e.all.harv = rowSums(.[12:22])) %>%
  ungroup()
# Yes -- neither e.all.harv nor d.unharv.inci exceed 1

foo$e.all.harv <- NULL


############################################################
## Add RRT with new col for each harv type x resist x resil x trans
# Do two intensities: x1 (which gets 0.2 all harv to RRT) or x2 (0.4)
# Plus re-allocation of any single tree selection
# See re-allocations in rrt_matrix.xls

rrt <- foo
colnames(rrt)
cols <- paste0(colnames(rrt[12:22]))
rrt <- cbind(rrt, setNames(rrt[cols], paste0("resistx1.", cols)))
rrt <- cbind(rrt, setNames(rrt[cols], paste0("resilx1.", cols)))
rrt <- cbind(rrt, setNames(rrt[cols], paste0("transx1.", cols)))
rrt <- cbind(rrt, setNames(rrt[cols], paste0("resistx2.", cols)))
rrt <- cbind(rrt, setNames(rrt[cols], paste0("resilx2.", cols)))
rrt <- cbind(rrt, setNames(rrt[cols], paste0("transx2.", cols)))


############################################################
## CHWP
chwp <- rrt %>%
  filter(for.type == "chwp")

## Rutland chwp public has 100% to crown thin. So need to change that back at end.
rutland <- chwp %>%
  filter(fips == 50021, own.type == "public")

chwp <- chwp %>%
  anti_join(rutland, by = c("fips", "own.type"))


#### Resist ####
# Intensities -- how much proportion gets re-allocated to RRT.
x1 <- 0.2
x2 <- 0.4

# x1. How much gets reallocated? All single tree and x1
add <- chwp$c.single.tree + x1
# How many treatments does realloation get distributed to?
newtx <- 2
chwp$resistx1.c.low.thin <- chwp$resistx1.c.low.thin + add/newtx
chwp$resistx1.c.crown.thin <- chwp$resistx1.c.crown.thin + add/newtx
chwp$resistx1.c.single.tree <- 0
chwp$resistx1.d.unharv.inci <- chwp$resistx1.d.unharv.inci - x1

# x2. How much gets reallocated? All single tree and x2
add <- chwp$c.single.tree + x2
# How many treatments does realloation get distributed to?
newtx <- 2
chwp$resistx2.c.low.thin <- chwp$resistx2.c.low.thin + add/newtx
chwp$resistx2.c.crown.thin <- chwp$resistx2.c.crown.thin + add/newtx
chwp$resistx2.c.single.tree <- 0
chwp$resistx2.d.unharv.inci <- chwp$resistx2.d.unharv.inci - x2


#### Resilience ####
# Intensities -- how much proportion gets re-allocated to RRT.
x1 <- 0.2
x2 <- 0.4

# x1. How much gets reallocated? All single tree and x1
add <- chwp$c.single.tree + x1
# How many treatments does realloation get distributed to?
newtx <- 2
chwp$resilx1.b.shelter <- chwp$resilx1.b.shelter + add/newtx
chwp$resilx1.b.seed.tree <- chwp$resilx1.b.seed.tree + add/newtx
chwp$resilx1.c.single.tree <- 0
chwp$resilx1.d.unharv.inci <- chwp$resilx1.d.unharv.inci - x1

# x2. How much gets reallocated? All single tree and x2
add <- chwp$c.single.tree + x2
# How many treatments does realloation get distributed to?
newtx <- 2
chwp$resilx2.b.shelter <- chwp$resilx2.b.shelter + add/newtx
chwp$resilx2.b.seed.tree <- chwp$resilx2.b.seed.tree + add/newtx
chwp$resilx2.c.single.tree <- 0
chwp$resilx2.d.unharv.inci <- chwp$resilx2.d.unharv.inci - x2  
  

#### Transition ####
# Intensities -- how much proportion gets re-allocated to RRT.
x1 <- 0.2
x2 <- 0.4

# x1. How much gets reallocated? All single tree and x1
add <- chwp$c.single.tree + x1
# How many treatments does realloation get distributed to?
newtx <- 3
chwp$transx1.b.shelter <- chwp$transx1.b.shelter + add/newtx
chwp$transx1.b.seed.tree <- chwp$transx1.b.seed.tree + add/newtx
chwp$transx1.a.grp.patch <- chwp$transx1.a.grp.patch + add/newtx
chwp$transx1.c.single.tree <- 0
chwp$transx1.d.unharv.inci <- chwp$transx1.d.unharv.inci - x1

# x2. How much gets reallocated? All single tree and x2
add <- chwp$c.single.tree + x2
# How many treatments does realloation get distributed to?
newtx <- 3
chwp$transx2.b.shelter <- chwp$transx2.b.shelter + add/newtx
chwp$transx2.b.seed.tree <- chwp$transx2.b.seed.tree + add/newtx
chwp$transx2.a.grp.patch <- chwp$transx2.a.grp.patch + add/newtx
chwp$transx2.c.single.tree <- 0
chwp$transx2.d.unharv.inci <- chwp$transx2.d.unharv.inci - x2  


## Tack Rutland back on.
chwp <- rbind(chwp, rutland)


## Check: proprtions of harv for x1 should sum to 1, so should x2. 1+1=2
(chwp %>% summarise(resist.all = rowSums(dplyr::select(., starts_with("resist")))) %>% range())
(chwp %>% summarise(resil.all = rowSums(dplyr::select(., starts_with("resil")))) %>% range())
(chwp %>% summarise(trans.all = rowSums(dplyr::select(., starts_with("trans")))) %>% range())
                          
## No RRT single.tree should have anything > 0. All unharv should be 0.8, 0.6 or lower 
(chwp %>% summarise(x = rowSums(dplyr::select(., contains("1.c.single.tree")))) %>% range())
(chwp %>% summarise(x = rowSums(dplyr::select(., contains("2.c.single.tree")))) %>% range())
(chwp %>% summarise(x = rowSums(dplyr::select(., contains("resistx1.d.unharv.inci")))) %>% range())
(chwp %>% summarise(x = rowSums(dplyr::select(., contains("resistx2.d.unharv.inci")))) %>% range())
(chwp %>% summarise(x = rowSums(dplyr::select(., contains("resilx1.d.unharv.inci")))) %>% range())
(chwp %>% summarise(x = rowSums(dplyr::select(., contains("resilx2.d.unharv.inci")))) %>% range())
(chwp %>% summarise(x = rowSums(dplyr::select(., contains("transx1.d.unharv.inci")))) %>% range())
(chwp %>% summarise(x = rowSums(dplyr::select(., contains("transx2.d.unharv.inci")))) %>% range())



####################################################################
## NHW
nhw <- rrt %>%
  filter(for.type == "nhw")


#### Resist ####
# Intensities -- how much proportion gets re-allocated to RRT.
x1 <- 0.2
x2 <- 0.4

# x1. How much gets reallocated? All single tree and x1
add <- nhw$c.single.tree + x1
# How many treatments does realloation get distributed to?
newtx <- 2
nhw$resistx1.c.low.thin <- nhw$resistx1.c.low.thin + add/newtx
nhw$resistx1.c.pct <- nhw$resistx1.c.pct + add/newtx
nhw$resistx1.c.single.tree <- 0
nhw$resistx1.d.unharv.inci <- nhw$resistx1.d.unharv.inci - x1

# x2. How much gets reallocated? All single tree and x2
add <- nhw$c.single.tree + x2
# How many treatments does realloation get distributed to?
newtx <- 2
nhw$resistx2.c.low.thin <- nhw$resistx2.c.low.thin + add/newtx
nhw$resistx2.c.pct <- nhw$resistx2.c.pct + add/newtx
nhw$resistx2.c.single.tree <- 0
nhw$resistx2.d.unharv.inci <- nhw$resistx2.d.unharv.inci - x2


#### Resilience ####
# Intensities -- how much proportion gets re-allocated to RRT.
x1 <- 0.2
x2 <- 0.4

# x1. How much gets reallocated? All single tree and x1
add <- nhw$c.single.tree + x1
# How many treatments does realloation get distributed to?
newtx <- 1
nhw$resilx1.b.shelter <- nhw$resilx1.b.shelter + add/newtx
nhw$resilx1.c.single.tree <- 0
nhw$resilx1.d.unharv.inci <- nhw$resilx1.d.unharv.inci - x1

# x2. How much gets reallocated? All single tree and x2
add <- nhw$c.single.tree + x2
# How many treatments does realloation get distributed to?
newtx <- 1
nhw$resilx2.b.shelter <- nhw$resilx2.b.shelter + add/newtx
nhw$resilx2.c.single.tree <- 0
nhw$resilx2.d.unharv.inci <- nhw$resilx2.d.unharv.inci - x2  



#### Transition ####
# Intensities -- how much proportion gets re-allocated to RRT.
x1 <- 0.2
x2 <- 0.4

# x1. How much gets reallocated? All single tree and x1
add <- nhw$c.single.tree + x1
# How many treatments does realloation get distributed to?
newtx <- 3
nhw$transx1.b.shelter <- nhw$transx1.b.shelter + add/newtx
nhw$transx1.b.seed.tree <- nhw$transx1.b.seed.tree + add/newtx
nhw$transx1.a.grp.patch <- nhw$transx1.a.grp.patch + add/newtx
nhw$transx1.c.single.tree <- 0
nhw$transx1.d.unharv.inci <- nhw$transx1.d.unharv.inci - x1

# x2. How much gets reallocated? All single tree and x2
add <- nhw$c.single.tree + x2
# How many treatments does realloation get distributed to?
newtx <- 3
nhw$transx2.b.shelter <- nhw$transx2.b.shelter + add/newtx
nhw$transx2.b.seed.tree <- nhw$transx2.b.seed.tree + add/newtx
nhw$transx2.a.grp.patch <- nhw$transx2.a.grp.patch + add/newtx
nhw$transx2.c.single.tree <- 0
nhw$transx2.d.unharv.inci <- nhw$transx2.d.unharv.inci - x2  


## Check: proprtions of harv for x1 should sum to 1, so should x2. 1+1=2
(nhw %>% summarise(resist.all = rowSums(dplyr::select(., starts_with("resist")))) %>% range())
(nhw %>% summarise(resil.all = rowSums(dplyr::select(., starts_with("resil")))) %>% range())
(nhw %>% summarise(trans.all = rowSums(dplyr::select(., starts_with("trans")))) %>% range())

## No RRT single.tree should have anything > 0. All unharv should be 0.8, 0.6 or lower 
(nhw %>% summarise(x = rowSums(dplyr::select(., contains("1.c.single.tree")))) %>% range())
(nhw %>% summarise(x = rowSums(dplyr::select(., contains("2.c.single.tree")))) %>% range())
(nhw %>% summarise(x = rowSums(dplyr::select(., contains("resistx1.d.unharv.inci")))) %>% range())
(nhw %>% summarise(x = rowSums(dplyr::select(., contains("resistx2.d.unharv.inci")))) %>% range())
(nhw %>% summarise(x = rowSums(dplyr::select(., contains("resilx1.d.unharv.inci")))) %>% range())
(nhw %>% summarise(x = rowSums(dplyr::select(., contains("resilx2.d.unharv.inci")))) %>% range())
(nhw %>% summarise(x = rowSums(dplyr::select(., contains("transx1.d.unharv.inci")))) %>% range())
(nhw %>% summarise(x = rowSums(dplyr::select(., contains("transx2.d.unharv.inci")))) %>% range())
  




####################################################################
## SF
sf <- rrt %>%
  filter(for.type == "sf")


#### Resist ####
# Intensities -- how much proportion gets re-allocated to RRT.
x1 <- 0.2
x2 <- 0.4

# x1. How much gets reallocated? All single tree and x1
add <- sf$c.single.tree + x1
# How many treatments does realloation get distributed to?
newtx <- 1
sf$resistx1.b.shelter <- sf$resistx1.b.shelter + add/newtx
sf$resistx1.c.single.tree <- 0
sf$resistx1.d.unharv.inci <- sf$resistx1.d.unharv.inci - x1

# x2. How much gets reallocated? All single tree and x2
add <- sf$c.single.tree + x2
# How many treatments does realloation get distributed to?
newtx <- 1
sf$resistx2.b.shelter <- sf$resistx2.b.shelter + add/newtx
sf$resistx2.c.single.tree <- 0
sf$resistx2.d.unharv.inci <- sf$resistx2.d.unharv.inci - x2


#### Resilience ####
# Intensities -- how much proportion gets re-allocated to RRT.
x1 <- 0.2
x2 <- 0.4

# x1. How much gets reallocated? All single tree and x1
add <- sf$c.single.tree + x1
# How many treatments does realloation get distributed to?
newtx <- 1
sf$resilx1.b.shelter <- sf$resilx1.b.shelter + add/newtx
sf$resilx1.c.single.tree <- 0
sf$resilx1.d.unharv.inci <- sf$resilx1.d.unharv.inci - x1

# x2. How much gets reallocated? All single tree and x2
add <- sf$c.single.tree + x2
# How many treatments does realloation get distributed to?
newtx <- 1
sf$resilx2.b.shelter <- sf$resilx2.b.shelter + add/newtx
sf$resilx2.c.single.tree <- 0
sf$resilx2.d.unharv.inci <- sf$resilx2.d.unharv.inci - x2  


#### Transition ####
# Intensities -- how much proportion gets re-allocated to RRT.
x1 <- 0.2
x2 <- 0.4

# x1. How much gets reallocated? All single tree and x1
add <- sf$c.single.tree + x1
# How many treatments does realloation get distributed to?
newtx <- 3
sf$transx1.b.shelter <- sf$transx1.b.shelter + add/newtx
sf$transx1.b.seed.tree <- sf$transx1.b.seed.tree + add/newtx
sf$transx1.a.grp.patch <- sf$transx1.a.grp.patch + add/newtx
sf$transx1.c.single.tree <- 0
sf$transx1.d.unharv.inci <- sf$transx1.d.unharv.inci - x1

# x2. How much gets reallocated? All single tree and x2
add <- sf$c.single.tree + x2
# How many treatments does realloation get distributed to?
newtx <- 3
sf$transx2.b.shelter <- sf$transx2.b.shelter + add/newtx
sf$transx2.b.seed.tree <- sf$transx2.b.seed.tree + add/newtx
sf$transx2.a.grp.patch <- sf$transx2.a.grp.patch + add/newtx
sf$transx2.c.single.tree <- 0
sf$transx2.d.unharv.inci <- sf$transx2.d.unharv.inci - x2  



## Check: proprtions of harv for x1 should sum to 1, so should x2. 1+1=2
(sf %>% summarise(resist.all = rowSums(dplyr::select(., starts_with("resist")))) %>% range())
(sf %>% summarise(resil.all = rowSums(dplyr::select(., starts_with("resil")))) %>% range())
(sf %>% summarise(trans.all = rowSums(dplyr::select(., starts_with("trans")))) %>% range())

## No RRT single.tree should have anything > 0. All unharv should be 0.8, 0.6 or lower 
(sf %>% summarise(x = rowSums(dplyr::select(., contains("1.c.single.tree")))) %>% range())
(sf %>% summarise(x = rowSums(dplyr::select(., contains("2.c.single.tree")))) %>% range())
(sf %>% summarise(x = rowSums(dplyr::select(., contains("resistx1.d.unharv.inci")))) %>% range())
(sf %>% summarise(x = rowSums(dplyr::select(., contains("resistx2.d.unharv.inci")))) %>% range())
(sf %>% summarise(x = rowSums(dplyr::select(., contains("resilx1.d.unharv.inci")))) %>% range())
(sf %>% summarise(x = rowSums(dplyr::select(., contains("resilx2.d.unharv.inci")))) %>% range())
(sf %>% summarise(x = rowSums(dplyr::select(., contains("transx1.d.unharv.inci")))) %>% range())
(sf %>% summarise(x = rowSums(dplyr::select(., contains("transx2.d.unharv.inci")))) %>% range())



#######################################################
## recombine and gather all harv (incl. RRT) into single column
rrt <- rbind(chwp, nhw, sf)

vars <- paste0(colnames(rrt[12:88]))
rrt <- rrt %>%
  pivot_longer(cols = all_of(vars), names_to = "harv.type", values_to = "ann.rate.avg")


## Add back on residual basal area and remove order (1st yr and hri don't matter)
lu.harv <- read.csv(paste0(data.dir, "lu_harv_rrt.csv"))

## Gather so forest type becomes variable
lu.harv <- lu.harv %>%
  pivot_longer(-c(1:2), # exclude colums harv class and remove order
               names_to = "for.reclass",
               values_to = "resid.ba")


## Join to data
rrt <- rrt %>%
  left_join(lu.harv, by = c("harv.type" = "harv.class",
                            "for.type" = "for.reclass"))



## Reorder, add units, and write
rrt <- rrt %>%
  dplyr::select(fips, county, state, tho.area.acre.avg, tho.area.acre.se,
                for.type, own.type, harv.type, remove.order, resid.ba.ft2ac = resid.ba, 
                ann.rate.avg, min.ba.select, stand.select)



currentDate <- Sys.Date()
write.csv(rrt, paste0(out.dir, "rrt_parametersXcountyXforXown_", currentDate, ".csv"))

