#### HARV RECLASS AND RATES ####

#######################################
#### VT PRODUCTION VS. CONSUMPTION ####
#######################################

## Load data
data <- read.csv(paste0(data.dir, "harv_summ_cntyXownXtype.csv")) # skip first row of long names
colnames(data)

## Gather so harv type becomes variable
temp <- data %>% gather(data, key = )
?gather
temp <- data %>% mutate(harv.grp = ifelse())


temp <- data %>%
  mutate(c1_whmale = ifelse(r1_whmale == "", 99,
                            ifelse(r1_whmale == paste0(levels(data$r1_whmale)[2]), 1, 0)))