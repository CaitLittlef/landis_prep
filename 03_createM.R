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
# value   count
# [1,]   121   66942 # sf pub
# [2,]   122  306958 # sf priv lrg
# [3,]   123   79584 # sf priv gte10
# [4,]   401  199820 # op pub
# [5,]   402   91515 # op priv lrg
# [6,]   403  224779 # op priv gte10
# [7,]   701    6261 # low pub
# [8,]   702    2790 # low pub
# [9,]   703   14886 # low priv lrg
# [10,]   801  691982# low priv gte10
# [11,]   802  653659# nhw pub
# [12,]   803  952598# nhw priv lrg
# [13,]    NA 9777226# nhw priv gte10



## Save
# writeRaster(ma, "MAs_ownXfor_12.tiff")



## Plot
# Get raster data ready for plotting
plot.data <- gplot_data(ma)


# Extend limits on brewer palettes with function getPalette below.
colorCount = length(unique(plot.data$value))
getPalette = colorRampPalette(brewer.pal(8, "RdYlBu"))
# In plot below, would apply as follows:
# scale_fill_manual(values = getPalette(colorCount), na.value = NA) +

# Or hard-code colors
display.brewer.pal(8, "Dark2")
(palette <- brewer.pal(8, "Dark2"))
# teal, orange, violet, yellow; should correspond w/ nhw, op, sf, low
# used color picker online to add dark and light to each shade
palette <- c("#4a4686", "#7570B3", "#aba8d1", # purple = sf
             "#974302", "#D95F02", "#fd913f", # orange = op
             "#a77b02", "#E6AB02", "#fdd053", # yellow = low
             "#126d52", "#1B9E77", "#2edca7") # teal = nhw
levels(factor(plot.data$value))
labels <- c("sf pub", "sf priv lrg", "sf priv med",
            "op pub", "op priv lrg", "op priv med",
            "low pub", "low priv lrg", "low priv med",
            "nhw pub", "nhw priv lrg", "nhw priv med")


## FIXME: ADD SCALE BAR & N-ARROW
# For scale bar and N-arrow
# install.packages('ggsn')
# library(ggsn)



g <- ggplot() + 
  geom_sf(data = ne.reg.dslv, color = "dark grey", fill = "#f0f0f0", size = 0.5) +
  geom_raster(data = plot.data[!is.na(plot.data$value),], # exclude NA from being plotted
              aes(x = x, y = y, fill = factor(value))) + # factor b/c it's continuous
  geom_sf(data = ne.lakes, color = "#80d0ff", fill = "#e5f6ff", size = 0.5) + 
  geom_sf(data = eco.ne.dslv, color = "dark grey", fill = NA, size = 0.5) + #"#fcfcfc") +
  scale_fill_manual("Strata by forest type & ownership",
                    values = palette,
                    na.value = NA,
                    labels = labels) +
  guides(fill=guide_legend(ncol=2)) +
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
        legend.background = element_rect(fill = "white", color = "black", size = 0,5),
        legend.justification=c("left", "top"), # which side oflegend position coords refer to
        legend.position=c(0,1), 
        legend.box.margin=ggplot2::margin(c(rep(5, 4))), # ggplot2 else draws from other package
        legend.text=element_text(size=10),
        legend.title = element_text(size=12)) #+
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

pdf("map_MAs_ownXfor_12.pdf", width = 6, height = 6, bg = "white")
g
dev.off()

