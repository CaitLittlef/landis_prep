## Process PADUS data to get avg. pub and priv land sizes.
# This only has PAs, so combine with Natl Woodland Owner Survey summaries
# Want public, private - lrg, private - sml

## Load states
NAmer <- st_read(dsn = "D:/Shared/BackedUp/Caitlin/boundaries/NorthAmer_StatesProvinces.shp") %>%
  st_buffer(dist = 0) # fix invalid geometries (warning re: lat/long vs. dd)
NAmer <- NAmer[!NAmer$NAME == "Guam",]
levels(NAmer$STATEABB)
ne.sts <- c("US-CT", "US-MA", "US-ME", "US-NH", "US-NJ", "US-NY", "US-PA", "US-RI", "US-VT")
ne <- NAmer[NAmer$STATEABB == "US-CT"|NAmer$STATEABB == "US-MA"|NAmer$STATEABB == "US-ME"|
              NAmer$STATEABB == "US-NH"|NAmer$STATEABB == "US-NJ"| NAmer$STATEABB == "US-NY"|
              NAmer$STATEABB == "US-PA"|NAmer$STATEABB == "US-RI"|NAmer$STATEABB == "US-VT" ,]
ne$COUNTRY <- droplevels(ne$COUNTRY)
ne$NAME <- droplevels(ne$NAME)
ne$STATEABB <- droplevels(ne$STATEABB)
plot(ne)



## Load in forest cover types
# src: https://data.fs.usda.gov/geodata/rastergateway/forest_type/ 

fortype <- raster("D:/Shared/BackedUp/Caitlin/ForestCoverType/conus_foresttype.img")
plot(fortype)

# Clip to study area (use it's own crs in mask so can avoid projecting raster)
ne.mask <- ne %>% st_transform(crs = paste0(crs(fortype))) %>% st_buffer(dist = 0)
plot(ne.mask)
for.ne <- fortype %>% crop(ne.mask) #%>% mask(ne.mask) 
# ^ Not continuing w/ mask b/c CT had some length > 0 FALSE.
plot(for.ne)

# Project back to lat/long
temp <- for.ne %>% projectRaster(crs = crs(ne))


zoom(for.ne)

?crop

st_is_valid(ne.mask)
paste0(crs(fortype))

crs(NAmer)
length(ne.mask)
st_write(vtnh, "D:/Shared/BackedUp/Caitlin/boundaries/vt_nh.shp")
st_write(ne, "D:/Shared/BackedUp/Caitlin/boundaries/ne.shp")


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
