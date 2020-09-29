## Load BAU parameters
bau <- read.csv(paste0(out.dir, "parametersXcountyXforXown_2020-09-03.csv"))

head(bau, 5)

# Reassociate with regions (aggregated counties) by which I defined THO area. 
counties <- read.csv(paste0(data.dir, "lu_counties.csv"))
# counties$fips <- as.factor(counties$fips)
counties$state <- as.factor(counties$state)
counties$county <- as.factor(counties$county)
counties$area_class <- as.factor(counties$area_class)

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

bau <- bau %>% left_join(counties[,c(1,4)], by = "fips")

bau.area.class.summ <- bau %>%
  group_by(for.type, own.type, area_class, harv.type) %>%
  summarize(ann.rate.avg = mean(ann.rate.avg),
            harv.return.int = mean(harv.return.int))

write.csv(bau.area.class.summ, paste0(out.dir, "bau.area.class.summ_", currentDate, ".csv"))

bau.summ <- bau %>%
  group_by(for.type, own.type, harv.type) %>%
  summarize(ann.rate.avg = mean(ann.rate.avg),
            harv.return.int = mean(harv.return.int))

write.csv(bau.summ, paste0(out.dir, "bau.summ_", currentDate, ".csv"))
