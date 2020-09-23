## Overlay public and private ownerships

######################### OWNERSHIPS ###################################################



## Load ownership
# src: https://www.fs.usda.gov/rds/archive/catalog/RDS-2017-0007 - Hewes, Jaketon H.; Butler, Brett J.; Liknes, Greg C. 2017. Forest ownership in the conterminous United States circa 2014: distribution of seven ownership types - geospatial dataset. Fort Collins, CO: Forest Service Research Data Archive. https://doi.org/10.2737/RDS-2017-0007
own <- raster(paste0(data.dir, "/RDS-2017-0007/Data/forown2016"))
plot(own)

# Clip to study area (use it's own crs in mask so can avoid projecting raster)
eco.ne.mask <- eco.ne %>%
  st_transform(crs = paste0(crs(own))) %>%
  st_buffer(dist = 0)
plot(eco.ne.mask)
own.ne <- own %>% crop(eco.ne.mask) %>% mask(eco.ne.mask)
plot(own.ne)
levels(own.ne)

# > levels(own.ne)
# [[1]]
# ID    COUNT OWNERSHIP_TYPE
# 0 80037780     NON-FOREST
# 1 13683707        FEDERAL
# 2  4125383          STATE
# 3   749752          LOCAL
# 4 16402213         FAMILY
# 5  7300505      CORPORATE
# 6   745337  OTHER-PRIVATE
# 7  1354249         TRIBAL


# Reclassify as simply public (1) vs. private (2)
own.ne.pp <- own.ne

own.ne.pp[own.ne.pp == 0] <- NA # set non-forest to NA
own.ne.pp[own.ne.pp == 2] <- 1
own.ne.pp[own.ne.pp == 3] <- 1
own.ne.pp[own.ne.pp == 4] <- 2
own.ne.pp[own.ne.pp == 5] <- 2
own.ne.pp[own.ne.pp == 6] <- 2
own.ne.pp[own.ne.pp == 7] <- 2

freq(own.ne.pp) 

plot(own.ne.pp)


# writeRaster(own.ne.pp, paste0(out.dir, "own.ne_", currentDate, ".tif"), format = "GTiff", overwrite = TRUE)

#########################################################################
## Combine with forest type

plot(for.ne)
plot(own.ne.pp)

freq(for.ne)
freq(own.ne.pp)

crs(for.ne)
crs(own.ne.pp)

res(for.ne)
res(own.ne.pp)

extent(for.ne)
extent(own.ne.pp) # shifted in x-axis

# But have tried a zillion times to extend, crop, mask by one another. Won't work.
# Resample instead
foo <- for.ne %>% resample(own.ne.pp, method = "ngb")
freq(foo)
all.equal(extent(own.ne.pp), extent(foo))
for.ne <- foo


f <-  sum(for.ne, own.ne.pp)
freq(f)



###############################################################################################################

## Plot
# Get raster data ready for plotting
plot.data <- gplot_data(f)


# Or hard-code colors
display.brewer.pal(8, "Dark2")
(palette <- brewer.pal(8, "Dark2"))
# teal, orange, violet, yellow; should correspond w/ nhw, op, sf, low
# used color picker online to add dark and light to each shade
palette <- c("#4a4686",  "#aba8d1", # purple = sf (120)
             "#974302", "#fd913f", # orange = op (400)
             "#126d52",  "#2edca7") # teal = nhw (800)
levels(factor(plot.data$value))
labels <- c("spruce-fir (public)", "spruce-fir (private)",
            "c. hardwood-pine (public)", "c. hardwood-pine (private)",
            "n. hardwood (public)", "n. hardwood (private")


## FIXME: ADD SCALE BAR & N-ARROW
# For scale bar and N-arrow
# install.packages('ggsn')
# library(ggsn)


# Highlgith study area; distinguish land from sea
eco.ne.dslv <- st_union(st_combine(eco.ne), by_feature = TRUE)
plot(eco.ne.dslv)

NAmer.dslv <- st_union(st_combine(NAmer), by_feature = TRUE)
plot(NAmer.dslv)


## Plot
g <- ggplot() + 
  geom_sf(data = NAmer.dslv, color = "dark grey", fill = "#f0f0f0", size = 0.5) +
  geom_raster(data = plot.data[!is.na(plot.data$value),], # exclude NA from being plotted
              aes(x = x, y = y, fill = factor(value))) + # factor b/c it's continuous
  geom_sf(data = ne.lakes, color = "#80d0ff", fill = "#e5f6ff", size = 0.5) + 
  geom_sf(data = eco.ne.dslv, color = "dark grey", fill = NA, size = 0.5) + #"#fcfcfc") +
  scale_fill_manual("Strata by forest type & ownership",
                    values = palette,
                    na.value = NA,
                    labels = labels) +
  guides(fill=guide_legend(ncol=1)) +
  coord_sf(xlim = c(1324033, 2257533),
           ylim = c(2137911, 3012911),
           # crs = st_crs(102003), # Albers Equal area # on means ne.reg won't plot??
           expand = FALSE) +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_line(color = "#fcfcfc"),
        panel.grid.minor = element_blank(),# blend lat/long into background
        panel.border = element_rect(fill = NA, color = "black", size = 0.5),
        panel.background = element_rect(fill = "#e5f6ff"),
        axis.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "dark grey", size = 0,5),
        legend.justification=c("left", "top"), # which side oflegend position coords refer to
        legend.position=c(0,1), 
        legend.box.margin=ggplot2::margin(c(rep(6, 4))), # ggplot2 else draws from other package
        legend.text=element_text(size=10),
        legend.title = element_text(size=11)) #+
# scalebar(location = "bottomright", dist = 100, dist_unit = "km",
#          x.min = 1324033, x.max = 2257533,
#          y.min = 2137911, y.max = 3012911)

g

# png("map_MAs_ownXfor_12.png", width = 480, height = 480, units = "px", bg = "white")
# g
# dev.off()

# tiff("map_MAs_ownXfor_12.tiff", width = 480, height = 480, bg = "white")
# g
# dev.off()

pdf(paste0(out.dir,"map_6forxown_", currentDate, ".pdf"), width = 6, height = 6, bg = "white")
g
dev.off()
