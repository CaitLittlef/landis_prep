## Load BAU parameters
bau <- read.csv(paste0(out.dir, "parametersXcountyXforXown_2020-10-22.csv"))

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


bau.area.class.summ <- bau %>%
  group_by(for.type, own.type, area_class_name, harv.type) %>%
  summarize(ann.rate.mean = mean(ann.rate.avg),
            ann.rate.sum = sum(ann.rate.avg),
            harv.return.int = mean(harv.return.int),
            nrecords = n()) %>%
  ungroup()

# write.csv(bau.area.class.summ, paste0(out.dir, "bau.area.class.summ_", currentDate, ".csv"))

bau.summ <- bau %>%
  group_by(for.type, own.type, harv.type) %>%
  summarize(ann.rate.mean = mean(ann.rate.avg),
            ann.rate.sum = sum(ann.rate.avg),
            harv.return.int = mean(harv.return.int),
            nrecords = n()) %>%
  ungroup()


# write.csv(bau.summ, paste0(out.dir, "bau.summ_", currentDate, ".csv"))


## Viz BAU
library(RColorBrewer)
display.brewer.all()
display.brewer.pal(8,"Set3")
palette <- brewer.pal(12, "Set3")
# palette <- colorRampPalette(brewer.pal(8, "Dark2"))(12) # extend to 12 harv types

# CAVEAT: These graphs do not distinguish between public and private.
# These are averaged aross counties equally so it is NOT area-weighted.
# counties x forest types x ownerships for which there is not FIA plot representation will get nearest neighbor.
# NHW predominate in area across the region.
# Prop stratum annually treated is basically annual harvest rate averaged across all FIA plots.
# So it's best to think about this in terms of within-stratum re-allocation for RRT.

#Faceted with area class
boo <- bau.area.class.summ
g <- ggplot(boo, aes(x = for.type, y = ann.rate.mean, fill = harv.type)) +
  geom_bar(stat = "identity", position = "fill") +#, position = "dodge") +# position = "fill) +
  facet_grid(. ~ area_class_name) +
  scale_fill_manual(values = palette, name = "Harvest type") +
  labs(x = "Forest tye", y = "Proportion annually treated \n(avg across counties & pub/priv)", color = "black") + 
  theme_bw() +
  theme(text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank())
g

v <- 1
pdf(paste0(out.dir,"BAU_summary_perc_",currentDate,"_v",v,".pdf"), height = 4, width = 7)
g ; dev.off() ; v <- v+1


foo <- bau.summ 
j <- ggplot(foo, aes(x = for.type, y = ann.rate.mean, fill = harv.type)) +
  # geom_col() +
  geom_bar(stat = "identity" , position = "dodge") +
  scale_fill_manual(values = palette, name = "Harvest type") +
  labs(x = "Forest tye", y = "Avg proportion annually treated \n(avg across counties & pub/priv)", color = "black") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank())
j



