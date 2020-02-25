######################### STUDY AREA ###################################################

## Load study area ecosections
# src: https://data.fs.usda.gov/geodata/rastergateway/forest_type/ 
eco.ne <- st_read(paste0(data.dir, "/StudyAreaProposal.shp"))
crs(eco.ne) ; proj.crs <- paste(crs(eco.ne))
# plot(eco.ne)

# Also create boundary
bbox <- as(extent(eco.ne), "SpatialPolygons") 
proj4string(bbox) <- paste0(proj.crs)
bbox <- st_as_sf(bbox)



######################### STATES ###################################################

## Load states (doesn't have county info)
NAmer <- st_read(dsn = "D:/Shared/BackedUp/Caitlin/boundaries/NorthAmer_StatesProvinces.shp") %>%
  st_buffer(dist = 0) # fix invalid geometries (warning re: lat/long vs. dd)
NAmer <- NAmer[!NAmer$NAME == "Guam",]
levels(NAmer$NAME)
ne.sts <- NAmer[NAmer$NAME == "Connecticut"|NAmer$NAME == "Maine"|NAmer$NAME == "Massachusetts"|
              NAmer$NAME == "New Hampshire"|NAmer$NAME == "New Jersey"| NAmer$NAME == "New York"|
              NAmer$NAME == "Pennsylvania"|NAmer$NAME == "Rhode Island"|NAmer$NAME == "Vermont" ,]
rm(NAmer)
ne.sts <- ne.sts %>% st_transform(crs = proj.crs) %>% st_buffer(dist = 0)
class(ne.sts)
plot(ne.sts)


## Load look-up for state fam forest areas from NWOS
lu.st <- read.csv("st_for_area.csv")
# Keep only m2 
lu.st <- lu.st[,c(1,2,7,8)]


######################### COUNTIES ###################################################

# ## Load counties
# counties <- st_read("D:/Shared/BackedUp/Caitlin/boundaries/tl_2017_us_county.shp")
# # Keep states in study area with FIPS codes; beware leading zeros lest CT gets dropped!
# counties <- st_transform(counties, crs = proj.crs)
# keeps.FIPS <- c("09", 23, 25, 33, 34, 36, 42, 44, 50)
# keeps.names <- c("CT", "ME", "MA", "NH", "NJ", "NY", "PA", "RI", "VT")
# ne.counties <- counties[counties$STATEFP %in% keeps.FIPS,]
# lu.FIPS <- data.frame(FIPS = keeps.FIPS, st_name = keeps.names)
# 
# # Keep only counties in study area;
# county <- st_crop(ne.counties, bbox) ; rm(counties, ne.counties)
# 
# # Keep only useful columns
# county <- county %>% dplyr::select(STATEFP, COUNTYFP, GEOID, NAME)
# county$GEOID <- droplevels(county$GEOID)




######################### FOREST TYPE ###################################################

## Load in forest cover and lu tbls. Grps are bigger; types are too specific.
# src: https://data.fs.usda.gov/geodata/rastergateway/forest_type/; https://data.fs.usda.gov/geodata/rastergateway/forest_type/conus_forest_type_metadata.php
# fortype <- raster(paste0(data.dir, "/for_cover_type/conus_foresttype.img"))
# plot(fortype)
# zoom(fortype)
forgrp <- raster(paste0(data.dir, "/for_cover_grp/conus_forestgroup.img"))
# plot(forgrp)
# zoom(forgrp)

# Load look-up table
lu.for <- read.csv(paste0(data.dir, "/for_cover_grp/for_grp_code.csv"),
                   head = FALSE, col.names = c("code", "for.grp"))


# Clip to study area (use it's own crs in mask so can avoid projecting raster)
eco.ne.mask <- eco.ne %>%
  st_transform(crs = paste0(crs(forgrp))) %>%
  st_buffer(dist = 0)
plot(eco.ne.mask)
for.ne <- forgrp %>% crop(eco.ne.mask) %>% mask(eco.ne.mask) 
plot(for.ne)
# zoom(for.ne)


# Set all zeros and non-relevant values to NA
for.ne[for.ne == 0] <- NA
# # Create template raster (all for pixels = 1)
# for.1 <- for.ne
# for.1 <- for.1*0+1
# plot(for.1)



# What's frequency of types?
# freq <- data.frame(freq(for.ne)) ; colnames(freq) <- c("code", "count")
# (freq <- left_join(freq, lu.for, by = "code"))
# 1	100	169556	White/Red/Jack Pine Group
# 2	120	483841	Spruce/Fir Group
# 3	160	9240	Loblolly/Shortleaf Pine Group
# 4	180	231	Pinyon/Juniper Group
# 5	200	85	Douglas-fir Group
# 6	380	1451	Exotic Softwoods Group
# 7	400	29218	Oak/Pine Group
# 8	500	475500	Oak/Hickory Group
# 9	600	514	Oak/Gum/Cypress Group
# 10	700	37645	Elm/Ash/Cottonwood Group
# 11	800	2572907	Maple/Beech/Birch Group
# 12	900	61716	Aspen/Birch Group
# 13	NA	9223362	NA


# Based on rough location (plot below), recode as follows.
for.ne[for.ne == 900] <- 800 #(aspen/birch to maple/beech/birch)
for.ne[for.ne == 100] <- 400 #(white/red/jack pine to oak/pine)
for.ne[for.ne == 160] <- 400 #(loblolly/shortleaf pine to oak/pine)
for.ne[for.ne == 180] <- 400 #(pinyon/juniper to oak/pine)
for.ne[for.ne == 200] <- 400 #(Doug-fir why?? to oak/pine)
for.ne[for.ne == 380] <- 400 #(exotic softwood to oak/pine)
for.ne[for.ne == 500] <- 400 #(oak/hickory to oak/pine)
for.ne[for.ne == 600] <- 400 #(oak/gum/cypress to oak/pine)

# Get raster data ready for plotting
plot.data <- gplot_data(for.ne)

palette <- c("green", "orange", "brown", "dark green")
g <- ggplot() + 
  geom_raster(data = plot.data, aes(x = x, y = y, fill = Class_Names)) +
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

freq(for.ne)

## FIXME: No idea why, but this writes only 120 and sets other forest types 400, 700, 8000 to NA
# writeRaster(for.ne, "for.4type.tif")
for.type.MA <- for.ne




## Cleanup
rm(forgrp, for.ne)
