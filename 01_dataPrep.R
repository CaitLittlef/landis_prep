## Process PADUS data to get avg. pub and priv land sizes.
# This only has PAs, so combine with Natl Woodland Owner Survey summaries
# Want public, private - lrg, private - sml




## Load study area ecosections
# src: https://data.fs.usda.gov/geodata/rastergateway/forest_type/ 
eco.ne <- st_read(paste0(data.dir, "/StudyAreaProposal.shp"))
crs(eco.ne) ; proj.crs <- paste(crs(eco.ne))
plot(eco.ne)


## Load states
NAmer <- st_read(dsn = "D:/Shared/BackedUp/Caitlin/boundaries/NorthAmer_StatesProvinces.shp") %>%
  st_buffer(dist = 0) # fix invalid geometries (warning re: lat/long vs. dd)
NAmer <- NAmer[!NAmer$NAME == "Guam",]
levels(NAmer$STATEABB)
ne.sts <- c("US-CT", "US-MA", "US-ME", "US-NH", "US-NJ", "US-NY", "US-PA", "US-RI", "US-VT")
ne <- NAmer[NAmer$STATEABB == "US-CT"|NAmer$STATEABB == "US-MA"|NAmer$STATEABB == "US-ME"|
              NAmer$STATEABB == "US-NH"|NAmer$STATEABB == "US-NJ"| NAmer$STATEABB == "US-NY"|
              NAmer$STATEABB == "US-PA"|NAmer$STATEABB == "US-RI"|NAmer$STATEABB == "US-VT" ,]
ne %>% st_transform(crs = proj.crs) %>% st_buffer(dist = 0)



## Load in forest cover types
fortype <- raster("D:/Shared/BackedUp/Caitlin/ForestCoverType/conus_foresttype.img")
plot(fortype)

# Clip to study area (use it's own crs in mask so can avoid projecting raster)
eco.ne.mask <- eco.ne %>% st_transform(crs = paste0(crs(fortype))) %>% st_buffer(dist = 0)
plot(eco.ne.mask)
for.ne <- fortype %>% crop(eco.ne.mask) %>% mask(eco.ne.mask) 
plot(for.ne)

# Set all zeros to NA
for.ne[for.ne <= 0] <- NA


# Project to desired crs and res (270 is what LANDIS will use)
for.ne <- for.ne %>% projectRaster(crs = proj.crs, res = c(270,270))
# Set 
for.ne[for.ne <= 0] <- NA
crs(for.ne)
res(for.ne) # weirdly rectangular pixels.



## Load in study area ecosections
# src: https://data.fs.usda.gov/geodata/edw/datasets.php?dsetCategory=geoscientificinformation

eco.ne <- st_read(paste0(data.dir, "/StudyAreaProposal.shp"))
plot(ecosec)
crs(ecosec)




## Load PADUS
# ref: https://www.usgs.gov/core-science-systems/science-analytics-and-synthesis/gap/pad-us-data-manual
# N.b., due to incompatible geometries, cannot just load desired layer:
# "PADUS2_0Combined_Fee_Designation_Easement".
# Must clean first.
padus.gdb <- "D:/Shared/BackedUp/Caitlin/PADUS2.0/PADUS2_0.gdb"
layer.list <- ogrListLayers(padus.gdb)

padus <- st_read(dsn = "D:/Shared/BackedUp/Caitlin/PADUS2.0/PADUS_fee_desig_esmt_CONUS_200131.shp")
valid = st_is_valid(padus)
st_buffer(padus[!is.na(valid)], 0.0)
padus <- 
padus <- padus %>% st_buffer(dist = 0) # fixes some invalid geometries
any(is.na(st_dimension(padus)))
any(length(padus)== 0)
st_is_valid(padus)
st_make_valid(padus)
?st_make_valid
