## Combine 4 forest types and 4 ownership types to create 16 MAs

# PREFERRED: run and load from from 01_genPrep.R and 02_ownPrep.R (tho own state merge won't work -- WHY???)
# b/c written fortype layer drops all but 120 and sets other types to NA -- WHY????

own.MA <- raster("own.all.gte10.tif") # looks ok
# for.type.MA <- raster("for.4type.tif") # only has for type 120 -- WHY????
for.type.MA.orig <- for.type.MA # for safe keeping else have to re-run for type script in 01_genPrep.R


plot(own.MA)
plot(for.type.MA)

freq(own.MA)
freq(for.type.MA)

# > freq(own.MA)
# value   count
# [1,]     1 1036942 # PUBLIC
# [2,]     2 1121522 # PRIV_LRG
# [3,]     3 1485287 # PRIV_GTE10
# [4,]    NA 9425249
# > freq(for.type.MA)
# value   count
# [1,]   120  483841 # Spruce/Fir Group
# [2,]   400  685795 # Oak/Pine Group
# [3,]   700   37645 # Elm/Ash/Cottonwood Group
# [4,]   800 2634623 # Maple/Beech/Birch Group
# [5,]    NA 9223362


# Crs match?
crs(own.MA)
crs(for.type.MA)

for.type.MA <- projectRaster(for.type.MA, own.MA, method = "ngb") # neighbor for computing any new values 

# extent match?
extent(for.type.MA)
extent(own.MA)


## Combine rasters into 12 MAs
ma <- sum(for.type.MA, own.MA)
plot(ma)
freq(ma)


## Save
writeRaster(ma, "MAs_ownXfor_12.tiff")



## Plot
# Get raster data ready for plotting
plot.data <- gplot_data(ma)

# Extend limits on brewer palettes with function getPalette below.
colorCount = length(unique(plot.data$value))
getPalette = colorRampPalette(brewer.pal(8, "RdYlBu"))

g <- ggplot() + 
  geom_raster(data = plot.data, aes(x = x, y = y, fill = factor(value))) + # factor b/c it's continuous
  # geom_sf(data = temp) + 
  scale_fill_manual(values = getPalette(colorCount), na.value = NA) +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),# blend lat/long into background
        panel.border = element_rect(fill = NA, color = "black", size = 0.5),
        # panel.background = element_rect(fill = "),
        axis.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black", size = 0,5))
g
