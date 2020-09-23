################################
################################
#### HARV RECLASS AND RATES ####
################################
################################


#################################################################################
#### INITIAL DATA PREP 
#################################################################################


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
lu.harv <- read.csv(paste0(data.dir, "lu_harv.csv")) %>% 
  dplyr::select(-remove.order, -nhw, -chwp, -sf)
  # dplyr::rename(resid.ba.nhw = nhw, resid.ba.chwp = chwp, resid.ba.sf = sf)
lu.for <- read.csv(paste0(data.dir, "lu_for.csv")) %>% dplyr::select(-for.type.name)



## Add county, state, harv reclass and forest type reclass
data <- data %>%
  left_join(lu.fips, by = "fips") %>%
  left_join(lu.harv, by = c("harv.type" = "harv.class")) %>%
  left_join(lu.for, by = "for.type")



## Reclass ownerships
data <- data %>%
  mutate(own.reclass = ifelse(owner == 10, "public", # FS
                            ifelse(owner == 20, "public", # other fed
                                   ifelse(owner == 30, "public", # state or local gvt
                                          "private")))) # all private



## Drop orig for and owner types
data <- data %>% dplyr::select(-owner, -for.type, -harv.reclass, -rx)



## Ttl plots specific to finer res (orig) county x owner x for type but not harv. Drop and use plot.cnts
# plot.cnts are specific to harv type AND county x own x for.
# But because I've re-classed owner, for type, and harv type, now need to combine plot.cnts.
data <- data %>%
  group_by(fips, county, state, yr.first, yr.last, for.reclass, own.reclass, harv.type) %>%
  summarise(plot.cnt = sum(plot.cnt)) %>%
  ungroup()



## Create new summary by county, forest, owner (i.e., mgmt unit = MU)
data <- data %>%
  group_by(fips, county, state, yr.first, yr.last, for.reclass, own.reclass) %>%
  mutate(plot.ttl.mu = sum(plot.cnt)) %>%
  ungroup()
# So now each record is effectively a management unit (MU)


Mode(data$yr.first)
Mode(data$yr.last)

  
################################################################################
#### COMPUTE AND BIN HARVESTING RATES
################################################################################

## Create MU-level summaries of prop harv by each type; annual rates; harv return interval
data <- data %>%
  mutate(prop.plots = plot.cnt/plot.ttl.mu) %>%
  mutate(ann.rate = prop.plots / (yr.last - yr.first + 1)) %>% #+1 for yrs inclusive
  # mutate(hri = ifelse(ann.rate == 0,NA,(1/ann.rate))) %>% # set NA if there's no harv ever.
  dplyr::select(-yr.first, -yr.last)
# So annual rate gives proportion of MU that ought to receive that type of harvest each year.



## Remove "dummy" records for harv classes that did not occur. 
# BUT retain d.all.resid to protect areas from harvest in case of any interpolation.
data.harv.only <- data %>%
  filter(! plot.cnt == 0) # nix any types with zero plots


## Is it reasonable that we don't paramterize low? How much harvest occurs there?
temp <- data.harv.only %>% filter(for.reclass == "low (excluded)",
                                  ! harv.type == "d.unharv", !harv.type == "d.inci")  
sum(temp$plot.cnt) # only 6 where some harvest took place. ok.
data <- data.harv.only %>% filter(!for.reclass == "low (excluded)")  



## To reduce number of MUs (currently 297), bin annual harv rates.
# Prior LANDIS projects have simulated out to 300 years ~ 0.003 ann rate 
hist(data$ann.rate, nclass = 20)
hist(data[data$ann.rate>0.003,]$ann.rate, nclass = 20)
hist(data[data$ann.rate>0.003 & data$ann.rate<0.125,]$ann.rate, nclass = 20)
range(data$ann.rate)



# ## Establish bins that capture annual rates greater than 0.003; bin remainder into 1 bin.
# # Note that shrinking bins mean each has fewer members so can't reduce to as few MUs
# (bins <- seq(0.0001,1.0001, by = 0.05)) # some will be empty, but that's ok.
# data$ann.rate.bin <- findInterval(data$ann.rate, vec = bins, rightmost.closed = FALSE)
# hist(data$ann.rate.bin[!data$ann.rate.bin == 0], n=20)



################################################################################
#### SET AVG HARV AREA BY COUNTY
################################################################################


## Counties have been binned into 4 avg harv area classes -- big to small.
# These are based roughly on harvest intensity (see Jacob's initial FIA-based map)
# And these are based on what I gleaned from the lit about actual operation size.
# Corporate generally > family and state generally > fed.
# But not all reports are by ownership type. So just do based on geography.
# The different harvest rates already reflect ownership differences.

## Binned as follows:
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


counties <- read.csv(paste0(data.dir, "lu_counties.csv"))
# counties$fips <- as.factor(counties$fips)
counties$state <- as.factor(counties$state)
counties$county <- as.factor(counties$county)
counties$area_class <- as.factor(counties$area_class)
keeps <- c("CT", "ME", "MA", "NH", "NJ", "NY", "PA", "RI", "VT")
counties <- counties %>% filter(state %in% keeps)
counties$state <- droplevels(counties$state)


## Iterate with values in hand (e.g., min, max) to generate values with reasonable mean and SE.
# Primarily change max, given likely skew towards smaller THOs


## informed by Kosiba & Duncan & Legaard & FPR (tho.area.4)
n <- 40 # no basis for picking this, but this will reflect greater SE.
min <- 8 # 
# max <- 445 # Coos but gives too high avg (even tho my avg is fairly unsubstantiated)
max <- 230
mean <- 120 # this is avg shelterwood in FPR

tho.area.4 <- runif(n, min = min, max = max) ; mean(tho.area.4) ; std.error(tho.area.4)
tho.area.4.avg <-  round(mean,0) #round(mean(tho.area.4),0)
tho.area.4.se <- round(std.error(tho.area.4),0)




## FPR for n VT. (tho.area.3)
n <- 40
mine <- 2
# max <- 481 # original. with this max, get avg of 240 -- way too high.
max <- 168
mean <- 84
med <- 30

tho.area.3 <- runif(n, min = min, max = max) ; mean(tho.area.3) ; std.error(tho.area.3)
tho.area.3.avg <-  round(mean,0) #round(mean(tho.area.3),0)
tho.area.3.se <- round(std.error(tho.area.3),0)




## FPR for s VT. (tho.area.2)
n <- 40
min <- 3
# max <- 200 # original. 
max <- 76 
mean <- 42
med <- 32

tho.area.2 <- runif(n, min = min, max = max) ; mean(tho.area.2) ; std.error(tho.area.2)
tho.area.2.avg <-  round(mean,0) #round(mean(tho.area.2),0)
tho.area.2.se <- round(std.error(tho.area.2),0)




## Kittredge NIPF for Quabbin. That region in tho.area.2, but NIPF may be rep of tho.area.1
# Plus, pixel size will be ~18 acres. So, can't have pixel size < that.
n <- 40
min <- 1
max <-  32
mean <- 16

tho.area.1 <- runif(n, min = min, max = max) ; mean(tho.area.1) ; std.error(tho.area.1)
tho.area.1.avg <- round(mean,0) #round(mean(tho.area.1),0)
tho.area.1.se <- round(std.error(tho.area.1),0)



## Attach to county data
colnames(counties)
counties <- counties %>%
  mutate(tho.area.avg = ifelse(area_class == 1, tho.area.1.avg,
                               ifelse(area_class == 2, tho.area.2.avg,
                                      ifelse(area_class == 3, tho.area.3.avg,
                                             tho.area.4.avg)))) %>%
  mutate(tho.area.se = ifelse(area_class == 1, tho.area.1.se,
                               ifelse(area_class == 2, tho.area.2.se,
                                      ifelse(area_class == 3, tho.area.3.se,
                                             tho.area.4.se))))

counties <- counties %>% dplyr::select(fips, tho.area.avg, tho.area.se)

## Tidy
remove(list = ls(pattern = "tho.area.")) # ls() gives list of objects that match pattern



## Attach to orig data
data <- data %>% left_join(counties, by = "fips")


################################################################################################
#### PARAMETERS FOR EACH MU
################################################################################################


## Now county identity doesn't matter, but will for mapping. Rather, compute rates and area grouped by...
# ... for type, own type, harv type. (Had also used ann rate bins, but that didn't end up reducing much)

data <- data %>%
  group_by(for.reclass, own.reclass, harv.type, tho.area.avg, tho.area.se) %>%
  mutate(ann.rate.avg = mean(ann.rate)) %>%
  mutate(hri = round(ifelse(ann.rate.avg == 0,NA,(1/ann.rate.avg)),0)) %>%
  ungroup() %>%
  dplyr::select(-plot.cnt, -plot.ttl.mu, -prop.plots, -ann.rate)



a <- nrow(data[data$harv.type == "d.all.resid",])
b <- nrow(data[!data$harv.type == "d.all.resid",])
a+b

n_distinct(data$fips)



################################################################################
#### SPECIFY RESIDUAL BA
################################################################################

## Set based on stocking guies
# Reload
lu.harv <- read.csv(paste0(data.dir, "lu_harv.csv")) %>%
  dplyr::select(-rx, -harv.reclass) %>%
  # filter(!harv.reclass == "d.all.resid") %>%
  distinct() # there are dupes given harv reclass.


## Gather so harv type becomes variable
lu.harv <- lu.harv %>%
  pivot_longer(-c(1:2), # exclude these columns
               names_to = "for.reclass",
               values_to = "resid.ba")


## Join to data
data <- data %>%
  left_join(lu.harv, by = c("harv.type" = "harv.class",
                            "for.reclass" = "for.reclass"))

## Add yr to implement. 
# On avg, Ethan's FIA-based data from plots measured from 2008-2015 (8 years inclusive)
# Randomly distribute harvest events within the first 8 years of model.
data$first.year <- sample(1:8, nrow(data), replace = TRUE)


## Stand selection (all random)
data$stand.select <- "random"



## Min BA threshold for consideration; followed by random selection
data <- data %>%
  mutate(min.ba.select = ifelse(for.reclass == "nhw", 90,
                                 ifelse(for.reclass == "chwp", 100, 110)))


  
  

## Reorder, add units, and write
data <- data %>%
  dplyr::select(fips, county, state, tho.area.avg, tho.area.se,
                for.reclass, own.reclass, harv.type, resid.ba, remove.order,
                ann.rate.avg, first.year, hri, min.ba.select, stand.select)


data <- data %>%
  rename(for.type = for.reclass,
         own.type = own.reclass,
         tho.area.acre.avg = tho.area.avg,
         tho.area.acre.se = tho.area.se,
         resid.ba.ft2ac = resid.ba,
         harv.return.int = hri)



currentDate <- Sys.Date()
write.csv(data, paste0(out.dir, "parametersXcountyXforXown_200903_", currentDate, ".csv"))


## In landscape:
# -remove areas > 3000 ft from consideration.
# -remove lowland/riparian forest type.
# -for areas without forest type or ownership represented here, apply nearest neighbor.

