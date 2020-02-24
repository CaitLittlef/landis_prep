## For each state, reduce fam forest to match 10+acre area from NWOS.
# Check each state area in raster to match NWOS (e.g., reduce PA b/c small chunk)
# Iteratively remove smallest patches til size achieved.

## FIXME: turn this into loop

# Clip forest ownerships to VT
r.vt <- own.ne %>%
  crop(ne.sts[ne.sts$NAME == "Vermont",]) %>%
  mask(ne.sts[ne.sts$NAME == "Vermont",])
plot(r.vt)

# Retain only family forest
# idk why direct select for fam for (4) won't work. 
# But need to take inverse (i.e., remove what's NOT 4)
r.vt[! r.vt == 4] <- NA
plot(r.vt)

# ^ Will do above, but test with county
r.chit <- own.ne %>%
  crop(county[county$NAME == "Chittenden",]) %>%
  mask(county[county$NAME == "Chittenden",])
r.chit[! r.chit == 4] <- NA
plot(r.chit)

# # Get list of all patches; could specify class = 4 here.
# lp <- get_patches(r.chit)
# lp <- get_patches(r.chit, return_raster = FALSE)

# Or forget grabbing patches b/c sieve may exist
# https://gis.stackexchange.com/questions/130993/remove-clumps-of-pixels-in-r
# N.b., for homemade rasters (matrices), patches may wrap around...
# necessitating extending raster with NA buffer (r2 <- extend(r, c(1,1)))

# Detect clumps of rasters and assign unique ID
plot(r.vt)
rc <- raster::clump(r.vt, directions = 8)
plot(rc)

# Get clump frequency table, which gives cell count   
f<-as.data.frame(freq(rc))
# m2 in forest should equals sum of those clump cell counts
sum(f$count) ; ncell(r.vt) # 674923 & 674923


# # Which rows of dataframe are of clumps of 10 or more cells?
# str(which(f$count <= 10))
# # Which clumps (i.e., raster values = clump ID) do these correspond to?
# str(f$value[which(f$count <= 10)])
# # Put these into a vector of clump ID to be removed
# excludeID <- f$value[which(f$count <= 10)]
# 
# # Make a new raster to be sieved, retaining only clumps of >10 cells.
# rsieve <- rc
# # assign NA to all clumps whose IDs are found in excludeID
# rsieve[rc %in% excludeID] <- NA
# 
# rsieve[rsieve %in% excludeID] <- NA
# 
# plot(rsieve)
# ncell(rsieve)


excludeID <- f[f$count < 100,]
excludeID <- as.vector(excludeID$value)

# Make a new raster to be sieved, retaining only clumps of >10 cells.
rsieve <- rc
# assign NA to all clumps whose IDs are found in excludeID
rsieve[rc %in% excludeID] <- NA

plot(rc)
zoom(rc)

plot(rsieve)
zoom(rsieve)


#extract IDs of clumps according to some criteria
clump9 = data.frame(freq(rc))
clump9 = clump9[ ! clump9$count < 9, ] #remove clump observations with frequency smaller than 9
clump9 = as.vector(clump9$value) # record IDs from clumps which met the criteria in previous step

rc[rc != clump9[1]] <- NA #replace cells with IDs which do not belong to the group of interest 

plot(rc)








## WHY DONT THESE NUMBERS MATCH??!
> lu.st[lu.st$st_name == "VT",]
st_FIPS st_name fam_for_area_gte1_m2 fam_for_area_gte10_m2
9      50      VT          11230036500           10202134060
> ncell(rsieve)*270*270
[1] 674923

674923*270*270
49201886700 - 10202134060


Allocate fato match mily land to >10 acres and <10 acres (too small) by county.
# Remove any that is too small.
# 1) By too-small clusters
# 2) Use contiguity index from landscapemetrics to classify counties.
# 3) For those that have very low contiguity (i.e., fragmented) assign high prop to too-small
# 4) For those that have very high contiguiuty (i.e., not framented) assign high prop to keep
# 5) ^ Base contiguiuty index on comparisons between known fragmented counties and non-fragmented (but lots fam and low corporate)
# 6) ^ Set all other forest to a set value? No -- just focus on family.




chittenden <- county %>%
  dplyr::filter(NAME == "Chittenden")
washington <- county %>%
  dplyr::filter(STATEFP == "50" & NAME == "Washington")
caledonia <- county %>%
  dplyr::filter(NAME == "Caledonia")
orleans <- county %>%
  dplyr::filter(STATEFP == "50" & NAME == "Orleans")


own.ne %>% mask(chittenden) %>% lsm_c_contig_mn()
own.ne %>% mask(washington) %>% lsm_c_contig_mn()
own.ne %>% mask(caledonia) %>% lsm_c_contig_mn()
own.ne %>% mask(orleans) %>% lsm_c_contig_mn()

own.ne %>% mask(chittenden) %>% lsm_c_area_mn()
own.ne %>% mask(chittenden) %>% lsm_c_area_sd()

own.ne %>% mask(orleans) %>% lsm_c_area_mn()
own.ne %>% mask(orleans) %>% lsm_c_area_sd()


york <- county %>%
  dplyr::filter(STATEFP == "23" & NAME == "York")
washington.me <- county %>%
  dplyr::filter(STATEFP == "23" & NAME == "Washington")

own.ne %>% mask(york) %>% lsm_c_area_mn()
own.ne %>% mask(washington.me) %>% lsm_c_area_mn()

own.ne %>% mask(york) %>% lsm_c_contig_mn()
own.ne %>% mask(washington.me) %>% lsm_c_contig_mn()

?lsm_c_area_mn()
lsm_c_area_sd()



lsm_c_contig_mn()
lsm_c_contig_mn()


https://www.jla-data.net/eng/creating-and-pruning-random-points-and-polygons/
  
  https://gis.stackexchange.com/questions/297852/calculating-statistics-per-area-for-categorical-raster-using-r
test <- raster::extract(for.ne, county)
