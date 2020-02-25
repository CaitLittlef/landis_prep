## For each state, reduce fam forest to match 10+acre area from NWOS.
# Check each state area in raster to match NWOS (e.g., reduce PA b/c small chunk)
# Iteratively remove smallest patches til size achieved.
# Note that for some states (e.g., Maine), forest raster is already too small relative to NWOS.
# In those cases, not removing ANY area and just setting rc --> final (instead of rsieve --> final)


######################### OWNERSHIPS ###################################################

## Load ownership
# src: https://www.fs.usda.gov/rds/archive/catalog/RDS-2017-0007 - Hewes, Jaketon H.; Butler, Brett J.; Liknes, Greg C. 2017. Forest ownership in the conterminous United States circa 2014: distribution of seven ownership types - geospatial dataset. Fort Collins, CO: Forest Service Research Data Archive. https://doi.org/10.2737/RDS-2017-0007
own <- raster(paste0(data.dir, "/RDS-2017-0007/Data/forown2016"))
plot(own)
# Save layer of only fam ownerships for future use
own.fam <- own
own.fam[! own.fam == 4] <- NA
# Clip to study area (use it's own crs in mask so can avoid projecting raster)
eco.ne.mask <- eco.ne %>%
  st_transform(crs = paste0(crs(own))) %>%
  st_buffer(dist = 0)
plot(eco.ne.mask)
own.ne <- own %>% crop(eco.ne.mask) %>% mask(eco.ne.mask)
plot(own.ne)

# writeRaster(own.ne, "own.ne.tif", overwrite=TRUE)
# own.ne <- raster("own.ne.tif") # Note that attrbiutes (e.g., ownership names get lost in save)

# zoom(own.ne)

levels(own.ne)
# [[1]]
# ID    COUNT OWNERSHIP_TYPE
# 1  0 80037780     NON-FOREST
# 2  1 13683707        FEDERAL
# 3  2  4125383          STATE
# 4  3   749752          LOCAL
# 5  4 16402213         FAMILY
# 6  5  7300505      CORPORATE
# 7  6   745337  OTHER-PRIVATE
# 8  7  1354249         TRIBAL


# Get raster data ready for plotting
plot.data <- gplot_data(own.ne)

palette <- brewer.pal(8, "Dark2")
g <- ggplot() + 
  geom_raster(data = plot.data, aes(x = x, y = y, fill = OWNERSHIP_TYPE)) +
  # geom_sf(data = temp) + 
  scale_fill_manual(values = palette, na.value = NA) +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),# blend lat/long into background
        panel.border = element_rect(fill = NA, color = "black", size = 0.5),
        # panel.background = element_rect(fill = "),
        axis.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black", size = 0,5))
g



##### FIXME: create loop for all states. N.b., only NJ, NH, NY, PA, RI have prop -- all should.


#################### CT ################################################################ 

state.name <- "Connecticut"
state.abb <- "CT"


# Clip forest ownerships to state
r <- own.ne %>%
  crop(ne.sts[ne.sts$NAME == paste0(state.name),]) %>%
  mask(ne.sts[ne.sts$NAME == paste0(state.name),])
plot(r)


# Retain only family forest; remove what's NOT 4, don't keep 4 (idk latter won't work)
r[! r == 4] <- NA
plot(r)


# B/c only using proportions of some states, scale target forest area in study area raster
# While full state area is...
full <- own.fam %>%
  crop(ne.sts[ne.sts$NAME == paste0(state.name),]) %>%
  mask(ne.sts[ne.sts$NAME == paste0(state.name),])
# Scale target by this proportion
(prop <- ncell(r)/ncell(full))


# Detect clumps of rasters and assign unique ID
rc <- raster::clump(r, directions = 4)
plot(rc)


# Get clump frequency table, which gives cell count   
f.rc<-as.data.frame(freq(rc))
# cells in forest should equals sum of those clump cell counts
# sum(f.rc$count[!is.na(f.rc$value)]); ncell(r[!is.na(r)])


# Get list of clumps that I want to exclude; trialed w/ various clump sizes
excludeID <- f.rc[f.rc$count < 459,] 
excludeID <- as.vector(excludeID$value); length(excludeID)


# Make a new raster to be sieved, retaining only bigger clumps.
rsieve <- rc
# Assign NA to all clumps whose IDs are found in excludeID
rsieve[rsieve %in% excludeID] <- NA
plot(rsieve)
# zoom(rsieve)


# How much area has been retained?
f.rsieve <- as.data.frame(freq(rsieve))
# sum(f.rsieve$count[!is.na(f.rsieve$value)]) ; ncell(rsieve[!is.na(rsieve)]) 
sieved <- sum(f.rsieve$count[!is.na(f.rsieve$value)])


# How does this compare to gte10 acre fam forest area in state -- our target? Res = 250x250m
trgt <- lu.st$fam_for_area_gte10_m2[lu.st$st_name == paste0(state.abb)]
trgt*prop - (sieved*250*250) 
# If negative, sieved (w/ forest removed) still has more forest than target
# If positive, sieved (w/ forest removed) now has too little land relative to target

# clumps of 459 leaves -3649760 <- keep b/c smaller differential
# clumps of 460 leaves 25037740

r.ct.fin <- rsieve*0+1


#################### ME ################################################################ 

state.name <- "Maine"
state.abb <- "ME"


# Clip forest ownerships to state
r <- own.ne %>%
  crop(ne.sts[ne.sts$NAME == paste0(state.name),]) %>%
  mask(ne.sts[ne.sts$NAME == paste0(state.name),])
plot(r)


# Retain only family forest; remove what's NOT 4, don't keep 4 (idk latter won't work)
r[! r == 4] <- NA
plot(r)


# B/c only using proportions of some states, scale target forest area in study area raster
# While full state area is...
full <- own.fam %>%
  crop(ne.sts[ne.sts$NAME == paste0(state.name),]) %>%
  mask(ne.sts[ne.sts$NAME == paste0(state.name),])
# Scale target by this proportion
(prop <- ncell(r)/ncell(full))


# Detect clumps of rasters and assign unique ID
rc <- raster::clump(r, directions = 4)
plot(rc)


# Get clump frequency table, which gives cell count   
f.rc<-as.data.frame(freq(rc))
# cells in forest should equals sum of those clump cell counts
sum(f.rc$count[!is.na(f.rc$value)]); ncell(r[!is.na(r)])


# Get list of clumps that I want to exclude; trialed w/ various clump sizes
excludeID <- f.rc[f.rc$count < 2,]
excludeID <- as.vector(excludeID$value)

# For this state, removing all clumps <2 still removes too much and I can't make smaller clumps.
# Randomize excludeIDs and then keep only half... or a quarter... or even fewer...
# excludeID <- sample(excludeID, length(excludeID)/2)
# excludeID <- sample(excludeID, length(excludeID)/4)
# excludeID <- sample(excludeID, length(excludeID)/10)
# excludeID <- sample(excludeID, length(excludeID)/50)
# excludeID <- sample(excludeID, length(excludeID)/200)
# excludeID <- sample(excludeID, length(excludeID)/500)
# excludeID <- sample(excludeID, length(excludeID)/2000)
# excludeID <- sample(excludeID, size = 1)


# Make a new raster to be sieved, retaining only bigger clumps.
rsieve <- rc
# Assign NA to all clumps whose IDs are found in excludeID
rsieve[rsieve %in% excludeID] <- NA
plot(rsieve)
# zoom(rsieve)


# How much area has been retained?
f.rsieve <- as.data.frame(freq(rsieve))
# sum(f.rsieve$count[!is.na(f.rsieve$value)]) ; ncell(rsieve[!is.na(rsieve)]) 
sieved <- sum(f.rsieve$count[!is.na(f.rsieve$value)])


# How does this compare to gte10 acre fam forest area in state -- our target? Res = 250x250m
trgt <- lu.st$fam_for_area_gte10_m2[lu.st$st_name == paste0(state.abb)]
trgt*prop - (sieved*250*250) 
# If negative, sieved (w/ for removed) still has more forest than target
# If positive, sieved (w/ for removed) now has too little land relative to target

# removing clumps of 1 (<2) leaves 491873520 -- too little land relative to target. 
# removing 1/2 clumps of 1 (<2) leaves 428873520
# removing 1/4 clumps of 1 (<2) leaves 397373520
# removing 1/20 clumps of 1 (<2) leaves 372123520
# removing 1/2000 clumps of 1 (<2) still leaves 365936020

# ^ Conclusion: this layer will never align with NWOS -- it already has too little land.
# So just keep raster as is and do not remove any clumps.

r.me.fin <- rc*0+1






#################### MA ################################################################ 

state.name <- "Massachusetts"
state.abb <- "MA"


# Clip forest ownerships to state
r <- own.ne %>%
  crop(ne.sts[ne.sts$NAME == paste0(state.name),]) %>%
  mask(ne.sts[ne.sts$NAME == paste0(state.name),])
plot(r)


# Retain only family forest; remove what's NOT 4, don't keep 4 (idk latter won't work)
r[! r == 4] <- NA
plot(r)


# Detect clumps of rasters and assign unique ID
rc <- raster::clump(r, directions = 4)
plot(rc)


# B/c only using proportions of some states, scale target forest area in study area raster
# While full state area is...
full <- own.fam %>%
  crop(ne.sts[ne.sts$NAME == paste0(state.name),]) %>%
  mask(ne.sts[ne.sts$NAME == paste0(state.name),])
# Scale target by this proportion
(prop <- ncell(r)/ncell(full))


# Get clump frequency table, which gives cell count   
f.rc<-as.data.frame(freq(rc))
# cells in forest should equals sum of those clump cell counts
# sum(f.rc$count[!is.na(f.rc$value)]); ncell(r[!is.na(r)])


# Get list of clumps that I want to exclude; trialed w/ various clump sizes
excludeID <- f.rc[f.rc$count < 67,]
excludeID <- as.vector(excludeID$value)


# Make a new raster to be sieved, retaining only bigger clumps.
rsieve <- rc
# Assign NA to all clumps whose IDs are found in excludeID
rsieve[rsieve %in% excludeID] <- NA
plot(rsieve)
# zoom(rsieve)


# How much area has been retained?
f.rsieve <- as.data.frame(freq(rsieve))
# sum(f.rsieve$count[!is.na(f.rsieve$value)]) ; ncell(rsieve[!is.na(rsieve)]) 
sieved <- sum(f.rsieve$count[!is.na(f.rsieve$value)])


# How does this compare to gte10 acre fam forest area in state -- our target? Res = 250x250m
trgt <- lu.st$fam_for_area_gte10_m2[lu.st$st_name == paste0(state.abb)]
trgt*prop - (sieved*250*250) 
# If negative, sieved (w/ for removed) still has more forest than target
# If positive, sieved (w/ for removed) now has too little land relative to target

# clumps of 67 leaves -61980 <-- keep b/c smaller differential
# clumps of 68 leaves 12500520


r.ma.fin <- rsieve*0+1


#################### NJ ################################################################ 

state.name <- "New Jersey"
state.abb <- "NJ"


# Clip forest ownerships to state
r <- own.ne %>%
  crop(ne.sts[ne.sts$NAME == paste0(state.name),]) %>%
  mask(ne.sts[ne.sts$NAME == paste0(state.name),])
plot(r)



# Retain only family forest; remove what's NOT 4, don't keep 4 (idk latter won't work)
r[! r == 4] <- NA
plot(r)



# B/c only using portion of NJ, will need to scale target forest area
# Whereas full NJ area is r <- own.ne %>%
full <- own.fam %>%
  crop(ne.sts[ne.sts$NAME == paste0(state.name),]) %>%
  mask(ne.sts[ne.sts$NAME == paste0(state.name),])
# Scale target by this proportion
(prop <- ncell(r)/ncell(full))



# Detect clumps of rasters and assign unique ID
rc <- raster::clump(r, directions = 4)
plot(rc)


# Get clump frequency table, which gives cell count   
f.rc<-as.data.frame(freq(rc))
# cells in forest should equals sum of those clump cell counts
# sum(f.rc$count[!is.na(f.rc$value)]); ncell(r[!is.na(r)])


# Get list of clumps that I want to exclude; trialed w/ various clump sizes
excludeID <- f.rc[f.rc$count < 5,]
excludeID <- as.vector(excludeID$value)


# Make a new raster to be sieved, retaining only bigger clumps.
rsieve <- rc
# Assign NA to all clumps whose IDs are found in excludeID
rsieve[rsieve %in% excludeID] <- NA
plot(rsieve)
# zoom(rsieve)


# How much area has been retained?
f.rsieve <- as.data.frame(freq(rsieve))
# sum(f.rsieve$count[!is.na(f.rsieve$value)]) ; ncell(rsieve[!is.na(rsieve)]) 
sieved <- sum(f.rsieve$count[!is.na(f.rsieve$value)])


# How does this compare to gte10 acre fam forest area in state -- our target? Res = 250x250m
trgt <- lu.st$fam_for_area_gte10_m2[lu.st$st_name == paste0(state.abb)]
trgt*prop - (sieved*250*250) 
# If negative, sieved (w/ for removed) still has more forest than target
# If positive, sieved (w/ for removed) now has too little land relative to target

# clumps of 4 leaves -5200299
# clumps of 5 leaves  2799701 <- keep b/c smaller differential


r.nj.fin <- rsieve*0+1


#################### NH ################################################################ 

state.name <- "New Hampshire"
state.abb <- "NH"


# Clip forest ownerships to state
r <- own.ne %>%
  crop(ne.sts[ne.sts$NAME == paste0(state.name),]) %>%
  mask(ne.sts[ne.sts$NAME == paste0(state.name),])
plot(r)



# Retain only family forest; remove what's NOT 4, don't keep 4 (idk latter won't work)
r[! r == 4] <- NA
plot(r)


# b/c only using proportions of some states, scale target forest area in study area raster
ncell(r) 
# While full state area is...
full <- own.fam %>%
  crop(ne.sts[ne.sts$NAME == paste0(state.name),]) %>%
  mask(ne.sts[ne.sts$NAME == paste0(state.name),])
ncell(full) 
# Scale target by this proportion
(prop <- ncell(r)/ncell(full))



# Detect clumps of rasters and assign unique ID
rc <- raster::clump(r, directions = 4)
plot(rc)


# Get clump frequency table, which gives cell count   
f.rc<-as.data.frame(freq(rc))
# cells in forest should equals sum of those clump cell counts
# sum(f.rc$count[!is.na(f.rc$value)]); ncell(r[!is.na(r)])


# Get list of clumps that I want to exclude; trialed w/ various clump sizes
excludeID <- f.rc[f.rc$count < 70,]
excludeID <- as.vector(excludeID$value)


# Make a new raster to be sieved, retaining only bigger clumps.
rsieve <- rc
# Assign NA to all clumps whose IDs are found in excludeID
rsieve[rsieve %in% excludeID] <- NA
plot(rsieve)
# zoom(rsieve)


# How much area has been retained?
f.rsieve <- as.data.frame(freq(rsieve))
# sum(f.rsieve$count[!is.na(f.rsieve$value)]) ; ncell(rsieve[!is.na(rsieve)]) 
sieved <- sum(f.rsieve$count[!is.na(f.rsieve$value)])


# How does this compare to gte10 acre fam forest area in state -- our target? Res = 250x250m
trgt <- lu.st$fam_for_area_gte10_m2[lu.st$st_name == paste0(state.abb)]
# Scale target by proportion of state include (will be *1 or 100% for New England)
trgt*prop - (sieved*250*250) 
# If negative, sieved (w/ for removed) still has more forest than target
# If positive, sieved (w/ for removed) now has too little land relative to target

# clumps of 4 leaves  -358344960
# clumps of 20 leaves -192844960
# clumps of 60 leaves -32094960
# clumps of 65 leaves -12407460
# clumps of 68 leaves -4157460 ; same 69
# clumps of 70 leaves   155040 <-- keep b/c smaller differential


r.nh.fin <- rsieve*0+1


#################### NY ################################################################ 

state.name <- "New York"
state.abb <- "NY"


# Clip forest ownerships to state
r <- own.ne %>%
  crop(ne.sts[ne.sts$NAME == paste0(state.name),]) %>%
  mask(ne.sts[ne.sts$NAME == paste0(state.name),])
plot(r)



# Retain only family forest; remove what's NOT 4, don't keep 4 (idk latter won't work)
r[! r == 4] <- NA
plot(r)


# B/c only using proportions of some states, scale target forest area in study area raster
# While full state area is...
full <- own.fam %>%
  crop(ne.sts[ne.sts$NAME == paste0(state.name),]) %>%
  mask(ne.sts[ne.sts$NAME == paste0(state.name),])
# Scale target by this proportion
(prop <- ncell(r)/ncell(full))


# Detect clumps of rasters and assign unique ID
rc <- raster::clump(r, directions = 4)
plot(rc)


# Get clump frequency table, which gives cell count   
f.rc<-as.data.frame(freq(rc))
# cells in forest should equals sum of those clump cell counts
# sum(f.rc$count[!is.na(f.rc$value)]); ncell(r[!is.na(r)])


# Get list of clumps that I want to exclude; trialed w/ various clump sizes
excludeID <- f.rc[f.rc$count < 2,]
excludeID <- as.vector(excludeID$value)


# For this state, removing all clumps of 1 (<2) still removes too much. 
# Randomize excludeIDs and then keep only half... or a quarter... or even fewer...
excludeID <- sample(excludeID, length(excludeID)/6000)
excludeID <- sample(excludeID, size = 1)


# Make a new raster to be sieved, retaining only bigger clumps.
rsieve <- rc
# Assign NA to all clumps whose IDs are found in excludeID
rsieve[rsieve %in% excludeID] <- NA
plot(rsieve)
# zoom(rsieve)


# How much area has been retained?
f.rsieve <- as.data.frame(freq(rsieve))
# sum(f.rsieve$count[!is.na(f.rsieve$value)]) ; ncell(rsieve[!is.na(rsieve)]) 
sieved <- sum(f.rsieve$count[!is.na(f.rsieve$value)])


# How does this compare to gte10 acre fam forest area in state -- our target? Res = 250x250m
trgt <- lu.st$fam_for_area_gte10_m2[lu.st$st_name == paste0(state.abb)]
# Scale target by proportion of state include (will be *1 or 100% for New England)
trgt*prop - (sieved*250*250) 
# If negative, sieved (w/ for removed) still has more forest than target
# If positive, sieved (w/ for removed) now has too little land relative to target

# Regardless of number of clumps removed (even 1 puny one), sieved still has too little land.
# ^ Conclusion: this layer will never align with NWOS -- it already has too little land.
# So just keep raster as is and do not remove any clumps.

r.ny.fin <- rc*0+1




#################### PA ################################################################ 

state.name <- "Pennsylvania"
state.abb <- "PA"


# Clip forest ownerships to state
r <- own.ne %>%
  crop(ne.sts[ne.sts$NAME == paste0(state.name),]) %>%
  mask(ne.sts[ne.sts$NAME == paste0(state.name),])
plot(r)



# Retain only family forest; remove what's NOT 4, don't keep 4 (idk latter won't work)
r[! r == 4] <- NA
plot(r)



# b/c only using proportions of some states, scale target forest area in study area raster
# While full state area is...
full <- own.fam %>%
  crop(ne.sts[ne.sts$NAME == paste0(state.name),]) %>%
  mask(ne.sts[ne.sts$NAME == paste0(state.name),])
# Scale target by this proportion
(prop <- ncell(r)/ncell(full))



# Detect clumps of rasters and assign unique ID
rc <- raster::clump(r, directions = 4)
plot(rc)


# Get clump frequency table, which gives cell count   
f.rc<-as.data.frame(freq(rc))
# cells in forest should equals sum of those clump cell counts
# sum(f.rc$count[!is.na(f.rc$value)]); ncell(r[!is.na(r)])


# Get list of clumps that I want to exclude; trialed w/ various clump sizes
excludeID <- f.rc[f.rc$count < 2,]
excludeID <- as.vector(excludeID$value)


# For New York, removing all clumps of 1 (<2) still removes too much. 
# Randomize excludeIDs and then keep only half... or a quarter... or even fewer...
excludeID <- sample(excludeID, length(excludeID)/6000)
excludeID <- sample(excludeID, size = 1)


# Make a new raster to be sieved, retaining only bigger clumps.
rsieve <- rc
# Assign NA to all clumps whose IDs are found in excludeID
rsieve[rsieve %in% excludeID] <- NA
plot(rsieve)
# zoom(rsieve)


# How much area has been retained?
f.rsieve <- as.data.frame(freq(rsieve))
# sum(f.rsieve$count[!is.na(f.rsieve$value)]) ; ncell(rsieve[!is.na(rsieve)]) 
sieved <- sum(f.rsieve$count[!is.na(f.rsieve$value)])


# How does this compare to gte10 acre fam forest area in state -- our target? Res = 250x250m
trgt <- lu.st$fam_for_area_gte10_m2[lu.st$st_name == paste0(state.abb)]
# Scale target by proportion of state include (will be *1 or 100% for New England)
trgt*prop - (sieved*250*250) 
# If negative, sieved (w/ for removed) still has more forest than target
# If positive, sieved (w/ for removed) now has too little land relative to target

# Regardless of number of clumps removed (even 1 puny one), sieved still has too little land.
# ^ Conclusion: this layer will never align with NWOS -- it already has too little land.
# So just keep raster as is and do not remove any clumps.

r.pa.fin <- rc*0+1



#################### RI ################################################################ 

state.name <- "Rhode Island"
state.abb <- "RI"

# Clip forest ownerships to state
r <- own.ne %>%
  crop(ne.sts[ne.sts$NAME == paste0(state.name),]) %>%
  mask(ne.sts[ne.sts$NAME == paste0(state.name),])
plot(r)


# Retain only family forest; remove what's NOT 4, don't keep 4 (idk latter won't work)
r[! r == 4] <- NA
plot(r)


# b/c only using proportions of some states, scale target forest area in study area raster
# While full state area is...
full <- own.fam %>%
  crop(ne.sts[ne.sts$NAME == paste0(state.name),]) %>%
  mask(ne.sts[ne.sts$NAME == paste0(state.name),])
# Scale target by this proportion
(prop <- ncell(r)/ncell(full))



# Detect clumps of rasters and assign unique ID
rc <- raster::clump(r, directions = 4)
plot(rc)


# Get clump frequency table, which gives cell count   
f.rc<-as.data.frame(freq(rc))
# cells in forest should equals sum of those clump cell counts
# sum(f.rc$count[!is.na(f.rc$value)]); ncell(r[!is.na(r)]) 


# Get list of clumps that I want to exclude; trialed w/ various clump sizes
excludeID <- f.rc[f.rc$count < 383,]
excludeID <- as.vector(excludeID$value) 


# Make a new raster to be sieved, retaining only bigger clumps.
rsieve <- rc
# Assign NA to all clumps whose IDs are found in excludeID
rsieve[rsieve %in% excludeID] <- NA
plot(rsieve)
# zoom(rsieve)

# How much area has been retained?
f.rsieve <- as.data.frame(freq(rsieve))
# sum(f.rsieve$count[!is.na(f.rsieve$value)]) ; ncell(rsieve[!is.na(rsieve)]) 
sieved <- sum(f.rsieve$count[!is.na(f.rsieve$value)])


# How does this compare to gte10 acre fam forest area in state -- our target? Res = 250x250m
trgt <- lu.st$fam_for_area_gte10_m2[lu.st$st_name == paste0(state.abb)]
trgt*prop - (sieved*250*250)
# If negative, sieved (w/ for removed) still has more forest than target
# If positive, sieved (w/ for removed) now has too little land relative to target

# remove clumps < 6: -181814000
# remove clumps < 382:-12939000
# remove clumps <383:  10936000 <- retain

# Stick with smaller differential.

r.ri.fin <- rsieve*0+1


#################### VT ################################################################ 

state.name <- "Vermont"
state.abb <- "VT"

# Clip forest ownerships to state
r <- own.ne %>%
  crop(ne.sts[ne.sts$NAME == paste0(state.name),]) %>%
  mask(ne.sts[ne.sts$NAME == paste0(state.name),])
plot(r)


# Retain only family forest; remove what's NOT 4, don't keep 4 (idk latter won't work)
r[! r == 4] <- NA
plot(r)


# b/c only using proportions of some states, scale target forest area in study area raster
# While full state area is...
full <- own.fam %>%
  crop(ne.sts[ne.sts$NAME == paste0(state.name),]) %>%
  mask(ne.sts[ne.sts$NAME == paste0(state.name),])
# Scale target by this proportion
(prop <- ncell(r)/ncell(full))



# Detect clumps of rasters and assign unique ID
rc <- raster::clump(r, directions = 4)
plot(rc)


# Get clump frequency table, which gives cell count   
f.rc<-as.data.frame(freq(rc))
# cells in forest should equals sum of those clump cell counts
# sum(f.rc$count[!is.na(f.rc$value)]); ncell(r[!is.na(r)]) 


# Get list of clumps that I want to exclude; trialed w/ various clump sizes
excludeID <- f.rc[f.rc$count < 6,]
excludeID <- as.vector(excludeID$value) 


# Make a new raster to be sieved, retaining only bigger clumps.
rsieve <- rc
# Assign NA to all clumps whose IDs are found in excludeID
rsieve[rsieve %in% excludeID] <- NA
plot(rsieve)
# zoom(rsieve)

# How much area has been retained?
f.rsieve <- as.data.frame(freq(rsieve))
# sum(f.rsieve$count[!is.na(f.rsieve$value)]) ; ncell(rsieve[!is.na(rsieve)]) #163452 & 163452
sieved <- sum(f.rsieve$count[!is.na(f.rsieve$value)])


# How does this compare to gte10 acre fam forest area in state -- our target? Res = 250x250m
trgt <- lu.st$fam_for_area_gte10_m2[lu.st$st_name == paste0(state.abb)]
trgt*prop - (sieved*250*250) 
# If negative, sieved (w/ for removed) still has more forest than target
# If positive, sieved (w/ for removed) now has too little land relative to target

# remove clumps < 6: -11740940, sieved (w/ for removed) still has more forest than target
# remove clumps < 7:   5134060, sieved (w/ for removed) now has too little land relative to target

# Stick with smaller differential.

r.vt.fin <- rsieve*0+1

rm(r, rc, rsieve, full)


#################### combine ################################################################


## Combine all state rasters into a single one
# List all final state rasters
(temp <- grep(".fin",names(.GlobalEnv),value=TRUE))
ls.r <- do.call("list",mget(temp))


###### FIXME: NO IDEA WHY THIS NO LONGER WORKS!!
# Merge all together 
fam <- do.call(merge, ls.r)
rm(ls.r)

# Set equal to 3 for categorization below
fam[!is.na(fam)] <- 3
plot(fam)

# writeRaster(m, "own.fam.gte10.tiff")



## Merge with original own.ne
levels(own.ne) # WHY ARE THESE DIFFERENT THAN freq(own.ne)???
# [[1]]
# ID    COUNT OWNERSHIP_TYPE
# 1  0 80037780     NON-FOREST ; set to NA
# 2  1 13683707        FEDERAL ; set to public (1)
# 3  2  4125383          STATE ; set to public (1)
# 4  3   749752          LOCAL ; set to public (1)
# 5  4 16402213         FAMILY ; set to priv_fam_gte10ac (3)
# 6  5  7300505      CORPORATE ; set to priv_xtra_lrg (2)
# 7  6   745337  OTHER-PRIVATE ; set to priv_xtra_lrg (2)
# 8  7  1354249         TRIBAL ; set to priv_xtra_lrg (2)


# Reclassify to 3 ownership types.
t <- own.ne
t[t == 0] <- NA
t[t == 1] <- 1
t[t == 2] <- 1
t[t == 3] <- 1
t[t == 4] <- 0 # b/c I'll be adding in redefined fam layer
t[t == 5] <- 2
t[t == 6] <- 2
t[t == 7] <- 2
plot(t)
plot(fam)


# Make sure extents match before adding
extent(t)
extent(fam)
fam <- extend(fam, t)


# Add rasters; remove NA else any input NA persists to output
t2 <- sum(t, fam, na.rm=T) 

# Any persistant zeros from t are too-small fam land. Set = NA
t2[t2 == 0] <- NA

plot(t2)

# writeRaster(t2, "own.all.gte10.tif")

own.MA <- t2
rm(fam, t, t2)
remove(list = ls(pattern = "r.$$.fin"))