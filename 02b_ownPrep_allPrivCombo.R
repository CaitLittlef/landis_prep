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
# Save layer of non-corpoate, non-public for slimming down by state. This will be for hitting fam forest trgts
own.fam <- own
own.fam[! own.fam == 4] <- NA
own.noncorp <- own
own.noncorp[own.noncorp < 4] <- NA #nix public (will be 1)
own.noncorp[own.noncorp == 5] <- NA #nix corporate (will be 2)
own.noncorp[own.noncorp > 0] <- 3 #set all remaining to 3 (will be 3)
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
# plot.data <- gplot_data(own.ne)

# palette <- brewer.pal(8, "Dark2")
# g <- ggplot() + 
#   geom_raster(data = plot.data, aes(x = x, y = y, fill = OWNERSHIP_TYPE)) +
#   # geom_sf(data = temp) + 
#   scale_fill_manual(values = palette, na.value = NA) +
#   theme_bw(base_size = 12) + 
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),# blend lat/long into background
#         panel.border = element_rect(fill = NA, color = "black", size = 0.5),
#         # panel.background = element_rect(fill = "),
#         axis.title = element_blank(),
#         legend.background = element_rect(fill = "white", color = "black", size = 0,5))
# g



############## private ratio 
## In prior code, had iteratively extracted area that roughly matched each state's area in <10acre family forest.
# Now, will simply fill landscape with larger or smaller avg. mgmt areas based on how heavily corporate priv forest is.
# Need tally of corporate:family for each county.




## Rasterize counties. Thse are the zones for zonal statistics.

## CANNOT GET FASTERIZE TO WORK ON COUNTIES. 
counties <- st_read("D:/Shared/BackedUp/Caitlin/boundaries/tl_2017_us_county.shp")

counties$geometry # MULTIPOLYGON
counties$GEOID <- as.numeric(counties$GEOID)
counties.r <- fasterize(counties, own.ne, field = "GEOID", fun = "first")
plot(counties.r) # gives blank.
levels(counties.r) # returns forest ownership values (i.e., raster template). WHY??!


# Try different class.
counties <- st_read("D:/Shared/BackedUp/Caitlin/boundaries/tl_2017_us_county.shp")
counties$GEOID <- as.numeric(counties$GEOID)
counties <- st_cast(counties, "POLYGON")
counties$geometry # POLYGON

counties.r <- fasterize(counties, own.ne, field = "GEOID", fun = "any")
plot(counties.r)  # gives blank.
levels(counties.r) # returns forest ownership values (i.e., raster template). WHY??!











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




# ## TO DO: REMOVE ALL AREAS THAT ARE IUCN CAT 1a1b (wilderness) -- having issues w/ CRS)
# ## Global database of PAs; for later removal of wilderness areas
# PA.all <- st_read(dsn = "D:/Shared/BackedUp/Caitlin/GlobalPAs/data/WDPA_Apr2019-shapefile/WDPA_Apr2019-shapefile-polygons.shp")
# 
# ## Will nix any IUCN categories 1a and 1b (=wilderness)
# PA.IaIb <- PA.all %>%
#   dplyr::select(NAME, IUCN_CAT) %>%
#   filter(IUCN_CAT == "Ia" | IUCN_CAT == "Ib") %>%
#   fasterize(for.ne) %>%
#   crop(for.ne) %>% mask(for.ne)
# crs(for.ne)
# plot(PA.IaIb)







own.MA <- t2
rm(fam, t, t2)
remove(list = ls(pattern = "r.*.fin"))
