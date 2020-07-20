#### HARV RECLASS AND RATES ####

#######################################
#### VT PRODUCTION VS. CONSUMPTION ####
#######################################

## Load data
data <- read.csv(paste0(data.dir, "harv_summ_cntyXownXtype.csv")) # skip first row of long names
colnames(data)

## Gather so harv type becomes variable
data <- data %>%
  pivot_longer(-c(1:4,17,18),
               names_to = "harv.type",
               values_to = "plot.cnt")

## Add in reclass of for type, harv type, owner
lu.harv <- read.csv(paste0(data.dir, "lu_harv.csv"))
lu.for <- read.csv(paste0(data.dir, "lu_for.csv")) %>% dplyr::select(-for.type.name)

data <- data %>%
  left_join(lu.harv, by = "harv.type") %>%
  left_join(lu.for, by = "for.type")

data <- data %>%
  mutate(own.class = ifelse(owner == 10, "public", # FS
                            ifelse(owner == 20, "public", # other fed
                                   ifelse(owner == 30, "public", # state or local gvt
                                          "private")))) # all private
