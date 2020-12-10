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
# But harvest-specific parameters (eg BA, remove order) makes this tricky. Remove for now.
foo <- bau %>%
  dplyr::select(-first.year, -resid.ba.ft2ac, -remove.order, -harv.return.int) %>%
  pivot_wider(names_from = harv.type, values_from = ann.rate.avg)

# So adding below works, set all NA to zero.
foo[is.na(foo)] <- 0

# Combine invi & unharv. 
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
# have slightly off numbers; 8 instances total. Nip/tuck from unharv to reconcile.


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
# Some portion of final allocations will get herbicide (chwp) and planting (all forest types).
# But some harvest types, even if they receive lots more in re-allocation, will be carry-overs...
# and so don't get plant or herb.
# Plus re-allocation of any single tree selection
# See re-allocations in rrt_matrix.xls

rrt <- foo
colnames(rrt)
cols <- paste0(colnames(rrt[12:22]))
rrt <- cbind(rrt, setNames(rrt[cols], paste0("resist.x1.", cols))) # place-holder for non-re-allocations, even if same tx.
rrt <- cbind(rrt, setNames(rrt[cols], paste0("resil.x1.", cols))) # place-holder for non-re-allocations, even if same tx.
rrt <- cbind(rrt, setNames(rrt[cols], paste0("trans.x1.", cols))) # place-holder for non-re-allocations, even if same tx.
rrt <- cbind(rrt, setNames(rrt[cols], paste0("resist.herb.x1.", cols))) # some re-allocations have herbicide
rrt <- cbind(rrt, setNames(rrt[cols], paste0("resil.herb.x1.", cols))) # some re-allocations have herbicide
rrt <- cbind(rrt, setNames(rrt[cols], paste0("trans.plant.x1.", cols))) # all re-allocation to trans have plant.
rrt <- cbind(rrt, setNames(rrt[cols], paste0("trans.plant.herb.x1.", cols))) # therefore plant, plant+herb & not herb alone.


rrt <- cbind(rrt, setNames(rrt[cols], paste0("resist.x2.", cols)))
rrt <- cbind(rrt, setNames(rrt[cols], paste0("resil.x2.", cols)))
rrt <- cbind(rrt, setNames(rrt[cols], paste0("trans.x2.", cols)))
rrt <- cbind(rrt, setNames(rrt[cols], paste0("resist.herb.x2.", cols)))
rrt <- cbind(rrt, setNames(rrt[cols], paste0("resil.herb.x2.", cols)))
rrt <- cbind(rrt, setNames(rrt[cols], paste0("trans.herb.x2.", cols)))
rrt <- cbind(rrt, setNames(rrt[cols], paste0("trans.plant.x2.", cols)))
rrt <- cbind(rrt, setNames(rrt[cols], paste0("trans.plant.herb.x2.", cols)))


# Set any her herbs or plants to having zero (non-herbs/plants keep vals). Will fill them in with re-allocations.
# Brute forcing it here with column references.

rrt[,56:99] <- 0
rrt[,133:187] <- 0


############################################################
############################################################
#### CHWP ####
############################################################
############################################################
chwp <- rrt %>%
  filter(for.type == "chwp")

## Rutland chwp public has 100% to crown thin. i.e., not enough unharv to re-alloate.
# So need to change that back at end.
rutland <- chwp %>%
  filter(fips == 50021, own.type == "public")

chwp <- chwp %>%
  anti_join(rutland, by = c("fips", "own.type"))


#### Resist ####
# Intensities -- how much proportion gets re-allocated to RRT. All single tree some unharv/inci.
x1 <- 0.2 ; addx1 <- chwp$c.single.tree + x1
x2 <- 0.4 ; addx2 <- chwp$c.single.tree + x2
# How many tx does reallocation get distributed to? With some public getting herbicide.
newtx <- 2

chwp <- chwp %>%
  # X1 
  mutate(resist.x1.c.low.thin = ifelse(own.type == "public",
                                      # If public, no-herb thin takes 90% of new allocation
                                      resist.x1.c.low.thin + addx1/newtx*0.9,
                                      # If not public, no-herb thin takes 100% of new allocation
                                      resist.x1.c.low.thin + addx1/newtx),
         resist.herb.x1.c.low.thin = ifelse(own.type == "public",
                                      # If public, new herb+thin gets 10% of new allocation     
                                      addx1/newtx*0.1,
                                      # If not public, new herb+thin gets no ne allocation
                                      0),
         resist.x1.c.crown.thin = ifelse(own.type == "public",
                                       resist.x1.c.crown.thin + addx1/newtx*0.9,
                                       resist.x1.c.crown.thin + addx1/newtx),
         resist.herb.x1.c.crown.thin = ifelse(own.type == "public",
                                              addx1/newtx*0.1,
                                              0),
         # Set all single-tree to zero.
         resist.x1.c.single.tree = 0,
         # Subtract proportion re-allocated from unharv.inci.
         resist.x1.d.unharv.inci = resist.x1.d.unharv.inci - x1, 
         
         # X2
         resist.x2.c.low.thin = ifelse(own.type == "public",
                                       resist.x2.c.low.thin + addx2/newtx*0.9,
                                       resist.x2.c.low.thin + addx2/newtx),
         resist.herb.x2.c.low.thin= ifelse(own.type == "public",
                                           addx2/newtx*0.1,
                                           0),
         resist.x2.c.crown.thin = ifelse(own.type == "public",
                                         resist.x2.c.crown.thin + addx2/newtx*0.9,
                                         resist.x2.c.crown.thin + addx2/newtx),
         resist.herb.x2.c.crown.thin = ifelse(own.type == "public",
                                              addx2/newtx*0.1,
                                              0),
         # Set all single-tree to zero.
         resist.x2.c.single.tree = 0,
         # Subtract proportion re-allocated from unharv.inci.
         resist.x2.d.unharv.inci = resist.x2.d.unharv.inci - x2)



#### Resilience ####
# Intensities -- how much proportion gets re-allocated to RRT. All single tree some unharv/inci.
x1 <- 0.2 ; addx1 <- chwp$c.single.tree + x1
x2 <- 0.4 ; addx2 <- chwp$c.single.tree + x2
# How many tx does reallocation get distributed to? With some public getting herbicide.
newtx <- 2

chwp <- chwp %>%
  # If public, then give NON-herb scenario 80% and herb scenario 20%.
  # If private, then give NON-herb scenario 100% and herb scenario 0%.
  # b/c of planting & herbicide, don't re-define existing tx levels; create new.
  # X1 
  mutate(resil.x1.b.shelter = ifelse(own.type == "public",
                                       resil.x1.b.shelter + addx1/newtx*0.8,
                                       resil.x1.b.shelter + addx1/newtx),
         resil.herb.x1.b.shelter= ifelse(own.type == "public",
                                           addx1/newtx*0.2,
                                           0),
         resil.x1.b.seed.tree = ifelse(own.type == "public",
                                         resil.x1.b.seed.tree + addx1/newtx*0.8,
                                         resil.x1.b.seed.tree + addx1/newtx),
         resil.herb.x1.b.seed.tree = ifelse(own.type == "public",
                                              addx1/newtx*0.2,
                                              0),
         # Set all single-tree to zero.
         resil.x1.c.single.tree = 0,
         # Subtract proportion re-allocated from unharv.inci.
         resil.x1.d.unharv.inci = resil.x1.d.unharv.inci - x1, 
         
         # X2
         resil.x2.b.shelter = ifelse(own.type == "public",
                                       resil.x2.b.shelter + addx2/newtx*0.8,
                                       resil.x2.b.shelter + addx2/newtx),
         resil.herb.x2.b.shelter= ifelse(own.type == "public",
                                           addx2/newtx*0.2,
                                           0),
         resil.x2.b.seed.tree = ifelse(own.type == "public",
                                         resil.x2.b.seed.tree + addx2/newtx*0.8,
                                         resil.x2.b.seed.tree + addx2/newtx),
         resil.herb.x2.b.seed.tree = ifelse(own.type == "public",
                                              addx2/newtx*0.2,
                                              0),
         # Set all single-tree to zero.
         resil.x2.c.single.tree = 0,
         # Subtract proportion re-allocated from unharv.inci.
         resil.x2.d.unharv.inci = resil.x2.d.unharv.inci - x2)



#### Transition ####
# Intensities -- how much proportion gets re-allocated to RRT. All single tree some unharv/inci.
x1 <- 0.2 ; addx1 <- chwp$c.single.tree + x1
x2 <- 0.4 ; addx2 <- chwp$c.single.tree + x2
# How many tx does reallocation get distributed to? With some public getting herbicide.
# Can't just add to existing tx, tho, because re-allocations get planting, too.
newtx <- 3

chwp <- chwp %>%
  # If public, then give NON-herb scenario 70% and herb scenario 30%.
  # If private, then give NON-herb scenario 100% and herb scenario 0%.
  # b/c of planting & herbicide, don't re-define existing tx levels; create new.
  # X1 
  mutate(trans.plant.x1.b.shelter = ifelse(own.type == "public",
                                     addx1/newtx*0.7,
                                     addx1/newtx),
         trans.plant.herb.x1.b.shelter= ifelse(own.type == "public",
                                        addx1/newtx*0.3,
                                        0),
         trans.plant.x1.b.seed.tree = ifelse(own.type == "public",
                                       addx1/newtx*0.7,
                                       addx1/newtx),
         trans.plant.herb.x1.b.seed.tree = ifelse(own.type == "public",
                                            addx1/newtx*0.3,
                                            0),
         trans.plant.x1.a.grp.patch = ifelse(own.type == "public",
                                       addx1/newtx*0.7,
                                       addx1/newtx),
         trans.plant.herb.x1.a.grp.patch = ifelse(own.type == "public",
                                            addx1/newtx*0.3,
                                            0),
         # Set all single-tree to zero.
         trans.x1.c.single.tree = 0,
         # Subtract proportion re-allocated from unharv.inci.
         trans.x1.d.unharv.inci = trans.x1.d.unharv.inci - x1, 
         
         # X2
         trans.plant.x2.b.shelter = ifelse(own.type == "public",
                                           addx2/newtx*0.7,
                                           addx2/newtx),
         trans.plant.herb.x2.b.shelter= ifelse(own.type == "public",
                                               addx2/newtx*0.3,
                                               0),
         trans.plant.x2.b.seed.tree = ifelse(own.type == "public",
                                             addx2/newtx*0.7,
                                             addx2/newtx),
         trans.plant.herb.x2.b.seed.tree = ifelse(own.type == "public",
                                                  addx2/newtx*0.3,
                                                  0),
         trans.plant.x2.a.grp.patch = ifelse(own.type == "public",
                                             addx2/newtx*0.7,
                                             addx2/newtx),
         trans.plant.herb.x2.a.grp.patch = ifelse(own.type == "public",
                                                  addx2/newtx*0.3,
                                                  0),
         # Set all single-tree to zero.
         trans.x2.c.single.tree = 0,
         # Subtract proportion re-allocated from unharv.inci.
         trans.x2.d.unharv.inci = trans.x2.d.unharv.inci - x2)



## Tack Rutland back on.
chwp <- rbind(chwp, rutland)


## Check: proprtions of harv for x1 should sum to 1, so should x2. 1+1=2
(chwp %>% summarise(resist.all = rowSums(dplyr::select(., starts_with("resist")))) %>% range())
(chwp %>% summarise(resil.all = rowSums(dplyr::select(., starts_with("resil")))) %>% range())
(chwp %>% summarise(trans.all = rowSums(dplyr::select(., starts_with("trans")))) %>% range())
                          
## No RRT single.tree should have anything > 0. All unharv should be 0.8, 0.6 or lower 
(chwp %>% summarise(x = rowSums(dplyr::select(., contains("1.c.single.tree")))) %>% range())
(chwp %>% summarise(x = rowSums(dplyr::select(., contains("2.c.single.tree")))) %>% range())
(chwp %>% summarise(x = rowSums(dplyr::select(., contains("resist.x1.d.unharv.inci")))) %>% range())
(chwp %>% summarise(x = rowSums(dplyr::select(., contains("resist.x2.d.unharv.inci")))) %>% range())
(chwp %>% summarise(x = rowSums(dplyr::select(., contains("resil.x1.d.unharv.inci")))) %>% range())
(chwp %>% summarise(x = rowSums(dplyr::select(., contains("resil.x2.d.unharv.inci")))) %>% range())
(chwp %>% summarise(x = rowSums(dplyr::select(., contains("trans.x1.d.unharv.inci")))) %>% range())
(chwp %>% summarise(x = rowSums(dplyr::select(., contains("trans.x2.d.unharv.inci")))) %>% range())


# temp <- chwp %>%
#   dplyr::select(county, state, for.type, own.type, starts_with("resil")) %>%
#   mutate(x = rowSums(dplyr::select(., starts_with("resil"), contains("x2"))))
# (range(temp$x))

# Check Rutland -- likely responsible for the only zeros in ranges above.  
temp <- t(chwp[192,])
# Yup -- only crown thinning happens under any scenario given re-allocation rules. So unharv.inci always zero.


####################################################################
####################################################################
#### NHW ####
####################################################################
####################################################################
## nb code mixed and diff than chwp b/c didn't have to edit for herbicide
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
nhw$resist.x1.c.low.thin <- nhw$resist.x1.c.low.thin + add/newtx
nhw$resist.x1.c.pct <- nhw$resist.x1.c.pct + add/newtx
nhw$resist.x1.c.single.tree <- 0
nhw$resist.x1.d.unharv.inci <- nhw$resist.x1.d.unharv.inci - x1

# x2. How much gets reallocated? All single tree and x2
add <- nhw$c.single.tree + x2
# How many treatments does realloation get distributed to?
newtx <- 2
nhw$resist.x2.c.low.thin <- nhw$resist.x2.c.low.thin + add/newtx
nhw$resist.x2.c.pct <- nhw$resist.x2.c.pct + add/newtx
nhw$resist.x2.c.single.tree <- 0
nhw$resist.x2.d.unharv.inci <- nhw$resist.x2.d.unharv.inci - x2


#### Resilience ####
# Intensities -- how much proportion gets re-allocated to RRT.
x1 <- 0.2
x2 <- 0.4

# x1. How much gets reallocated? All single tree and x1
add <- nhw$c.single.tree + x1
# How many treatments does realloation get distributed to?
newtx <- 1
nhw$resil.x1.b.shelter <- nhw$resil.x1.b.shelter + add/newtx
nhw$resil.x1.c.single.tree <- 0
nhw$resil.x1.d.unharv.inci <- nhw$resil.x1.d.unharv.inci - x1

# x2. How much gets reallocated? All single tree and x2
add <- nhw$c.single.tree + x2
# How many treatments does realloation get distributed to?
newtx <- 1
nhw$resil.x2.b.shelter <- nhw$resil.x2.b.shelter + add/newtx
nhw$resil.x2.c.single.tree <- 0
nhw$resil.x2.d.unharv.inci <- nhw$resil.x2.d.unharv.inci - x2  



#### Transition ####
# Intensities -- how much proportion gets re-allocated to RRT. All single tree some unharv/inci.
x1 <- 0.2 ; addx1 <- nhw$c.single.tree + x1
x2 <- 0.4 ; addx2 <- nhw$c.single.tree + x2
# How many tx does reallocation get distributed to? With some public getting herbicide.
# Can't just add to existing tx, tho, because re-allocations get planting, too.
newtx <- 3

nhw <- nhw %>%
  # b/c of planting, don't re-define existing tx levels; create new.
  # X1 
  mutate(trans.plant.x1.b.shelter = addx1/newtx,
         trans.plant.x1.b.seed.tree = addx1/newtx,
         trans.plant.x1.a.grp.patch = addx1/newtx,

         # Set all single-tree to zero.
         trans.x1.c.single.tree = 0,
         # Subtract proportion re-allocated from unharv.inci.
         trans.x1.d.unharv.inci = trans.x1.d.unharv.inci - x1, 
         
         # X2
         trans.plant.x2.b.shelter = addx2/newtx,
         trans.plant.x2.b.seed.tree = addx2/newtx,
         trans.plant.x2.a.grp.patch = addx2/newtx,

         # Set all single-tree to zero.
         trans.x2.c.single.tree = 0,
         # Subtract proportion re-allocated from unharv.inci.
         trans.x2.d.unharv.inci = trans.x2.d.unharv.inci - x2)


## Check: proprtions of harv for x1 should sum to 1, so should x2. 1+1=2
(nhw %>% summarise(resist.all = rowSums(dplyr::select(., starts_with("resist")))) %>% range())
(nhw %>% summarise(resil.all = rowSums(dplyr::select(., starts_with("resil")))) %>% range())
(nhw %>% summarise(trans.all = rowSums(dplyr::select(., starts_with("trans")))) %>% range())

## No RRT single.tree should have anything > 0. All unharv should be 0.8, 0.6 or lower 
(nhw %>% summarise(x = rowSums(dplyr::select(., contains("1.c.single.tree")))) %>% range())
(nhw %>% summarise(x = rowSums(dplyr::select(., contains("2.c.single.tree")))) %>% range())
(nhw %>% summarise(x = rowSums(dplyr::select(., contains("resist.x1.d.unharv.inci")))) %>% range())
(nhw %>% summarise(x = rowSums(dplyr::select(., contains("resist.x2.d.unharv.inci")))) %>% range())
(nhw %>% summarise(x = rowSums(dplyr::select(., contains("resil.x1.d.unharv.inci")))) %>% range())
(nhw %>% summarise(x = rowSums(dplyr::select(., contains("resil.x2.d.unharv.inci")))) %>% range())
(nhw %>% summarise(x = rowSums(dplyr::select(., contains("trans.x1.d.unharv.inci")))) %>% range())
(nhw %>% summarise(x = rowSums(dplyr::select(., contains("trans.x2.d.unharv.inci")))) %>% range())
  



####################################################################
####################################################################
#### SF #### 
####################################################################
####################################################################
## nb code mixed and diff than chwp b/c didn't have to edit for herbicide
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
sf$resist.x1.b.shelter <- sf$resist.x1.b.shelter + add/newtx
sf$resist.x1.c.single.tree <- 0
sf$resist.x1.d.unharv.inci <- sf$resist.x1.d.unharv.inci - x1

# x2. How much gets reallocated? All single tree and x2
add <- sf$c.single.tree + x2
# How many treatments does realloation get distributed to?
newtx <- 1
sf$resist.x2.b.shelter <- sf$resist.x2.b.shelter + add/newtx
sf$resist.x2.c.single.tree <- 0
sf$resist.x2.d.unharv.inci <- sf$resist.x2.d.unharv.inci - x2


#### Resilience ####
# Intensities -- how much proportion gets re-allocated to RRT.
x1 <- 0.2
x2 <- 0.4

# x1. How much gets reallocated? All single tree and x1
add <- sf$c.single.tree + x1
# How many treatments does realloation get distributed to?
newtx <- 1
sf$resil.x1.b.shelter <- sf$resil.x1.b.shelter + add/newtx
sf$resil.x1.c.single.tree <- 0
sf$resil.x1.d.unharv.inci <- sf$resil.x1.d.unharv.inci - x1

# x2. How much gets reallocated? All single tree and x2
add <- sf$c.single.tree + x2
# How many treatments does realloation get distributed to?
newtx <- 1
sf$resil.x2.b.shelter <- sf$resil.x2.b.shelter + add/newtx
sf$resil.x2.c.single.tree <- 0
sf$resil.x2.d.unharv.inci <- sf$resil.x2.d.unharv.inci - x2  


#### Transition ####
# Intensities -- how much proportion gets re-allocated to RRT. All single tree some unharv/inci.
x1 <- 0.2 ; addx1 <- sf$c.single.tree + x1
x2 <- 0.4 ; addx2 <- sf$c.single.tree + x2
# How many tx does reallocation get distributed to? With some public getting herbicide.
# Can't just add to existing tx, tho, because re-allocations get planting, too.
newtx <- 3

sf <- sf %>%
  # b/c of planting, don't re-define existing tx levels; create new.
  # X1 
  mutate(trans.plant.x1.b.shelter = addx1/newtx,
         trans.plant.x1.b.seed.tree = addx1/newtx,
         trans.plant.x1.a.grp.patch = addx1/newtx,
         
         # Set all single-tree to zero.
         trans.x1.c.single.tree = 0,
         # Subtract proportion re-allocated from unharv.inci.
         trans.x1.d.unharv.inci = trans.x1.d.unharv.inci - x1, 
         
         # X2
         trans.plant.x2.b.shelter = addx2/newtx,
         trans.plant.x2.b.seed.tree = addx2/newtx,
         trans.plant.x2.a.grp.patch = addx2/newtx,
         
         # Set all single-tree to zero.
         trans.x2.c.single.tree = 0,
         # Subtract proportion re-allocated from unharv.inci.
         trans.x2.d.unharv.inci = trans.x2.d.unharv.inci - x2) 



## Check: proprtions of harv for x1 should sum to 1, so should x2. 1+1=2
(sf %>% summarise(resist.all = rowSums(dplyr::select(., starts_with("resist")))) %>% range())
(sf %>% summarise(resil.all = rowSums(dplyr::select(., starts_with("resil")))) %>% range())
(sf %>% summarise(trans.all = rowSums(dplyr::select(., starts_with("trans")))) %>% range())

## No RRT single.tree should have anything > 0. All unharv should be 0.8, 0.6 or lower 
(sf %>% summarise(x = rowSums(dplyr::select(., contains("1.c.single.tree")))) %>% range())
(sf %>% summarise(x = rowSums(dplyr::select(., contains("2.c.single.tree")))) %>% range())
(sf %>% summarise(x = rowSums(dplyr::select(., contains("resist.x1.d.unharv.inci")))) %>% range())
(sf %>% summarise(x = rowSums(dplyr::select(., contains("resist.x2.d.unharv.inci")))) %>% range())
(sf %>% summarise(x = rowSums(dplyr::select(., contains("resil.x1.d.unharv.inci")))) %>% range())
(sf %>% summarise(x = rowSums(dplyr::select(., contains("resil.x2.d.unharv.inci")))) %>% range())
(sf %>% summarise(x = rowSums(dplyr::select(., contains("trans.x1.d.unharv.inci")))) %>% range())
(sf %>% summarise(x = rowSums(dplyr::select(., contains("trans.x2.d.unharv.inci")))) %>% range())



#######################################################
#######################################################
#### FINAL SUMMARY ####
#######################################################
#######################################################
## recombine and gather all harv (incl. RRT) into single column
rrt <- rbind(chwp, nhw, sf)

colnames(rrt)

vars <- paste0(colnames(rrt[12:187]))
rrt <- rrt %>%
  pivot_longer(cols = all_of(vars), names_to = "harv.type", values_to = "ann.rate.avg")

rrt %>% group_by(ann.rate.avg) %>% tally(ann.rate.avg == 0) # gives 67951 zeros


## Add back on residual basal area and remove order (1st yr and hri don't matter)
lu.harv <- read.csv(paste0(data.dir, "lu_harv_rrt_v2.csv"))

## Gather so forest type becomes variable
lu.harv <- lu.harv %>%
  pivot_longer(-c(1:2), # exclude colums harv class and remove order
               names_to = "for.reclass",
               values_to = "resid.ba")


## Join to data
rrt <- rrt %>%
  left_join(lu.harv, by = c("harv.type" = "harv.class",
                            "for.type" = "for.reclass"))



## Reorder, add units.
rrt <- rrt %>%
  dplyr::select(fips, county, state, tho.area.acre.avg, tho.area.acre.se,
                for.type, own.type, harv.type, remove.order, resid.ba.ft2ac = resid.ba, 
                ann.rate.avg, min.ba.select, stand.select)
# size of vars * cols: 80256 * 13 = 1043328
length(unique(rrt$harv.type)) # should have 176 harv types


## Can pivot wider but then it seemingly loses some cells
rrt <- rrt %>%
  pivot_wider(names_from = "harv.type", values_from = "ann.rate.avg")
# # size of vars * cols: 5472 * 187 = 1023264.

# Pivoting wider gives lots of NAs, presumably where there had been zero for ann rates. Confirm ok.
rrt[,12:187][is.na(rrt[,12:187])] <- 0 # have na as placeholder for resid ba and remove order; skip those columns


## Check: proprtions of harv for x1 should sum to 1, so should x2. 1+1=2 (or 1 for BAU)
(rrt %>% summarise(resist.all = rowSums(dplyr::select(., starts_with("resist")))) %>% range())
(rrt %>% summarise(resil.all = rowSums(dplyr::select(., starts_with("resil")))) %>% range())
(rrt %>% summarise(trans.all = rowSums(dplyr::select(., starts_with("trans")))) %>% range())
position <- c(12:22) ; (rrt %>% summarise(x = rowSums(dplyr::select(., position))) %>% range()) ; rm(position)

## No RRT single.tree should have anything > 0. All unharv should be 0.8, 0.6 or lower 
(rrt %>% summarise(x = rowSums(dplyr::select(., contains("1.c.single.tree")))) %>% range())
(rrt %>% summarise(x = rowSums(dplyr::select(., contains("2.c.single.tree")))) %>% range())
(rrt %>% summarise(x = rowSums(dplyr::select(., contains("resist.x1.d.unharv.inci")))) %>% range())
(rrt %>% summarise(x = rowSums(dplyr::select(., contains("resist.x2.d.unharv.inci")))) %>% range())
(rrt %>% summarise(x = rowSums(dplyr::select(., contains("resil.x1.d.unharv.inci")))) %>% range())
(rrt %>% summarise(x = rowSums(dplyr::select(., contains("resil.x2.d.unharv.inci")))) %>% range())
(rrt %>% summarise(x = rowSums(dplyr::select(., contains("trans.x1.d.unharv.inci")))) %>% range())
(rrt %>% summarise(x = rowSums(dplyr::select(., contains("trans.x2.d.unharv.inci")))) %>% range())



currentDate <- Sys.Date()
write.csv(rrt, paste0(out.dir, "rrt_parametersXcountyXforXown_", currentDate, ".csv"))

