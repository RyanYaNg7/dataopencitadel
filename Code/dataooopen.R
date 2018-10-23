# load data
library(readr)
library(dplyr)
library(stringr)
library(data.table)

chemicals <- read_csv("Desktop/dataopen/chemicals.csv") %>% as.data.table()
industry_occupation <- read_csv("Desktop/dataopen/industry_occupation.csv") %>% as.data.table()
water_usage <- read_csv("Desktop/dataopen/water_usage.csv") %>% as.data.table()
water_usage_dictionary <- read_csv("Desktop/dataopen/water_usage_dictionary.csv") %>% as.data.table()
earnings <- read_csv("Desktop/dataopen/earnings.csv") %>% as.data.table()


# exploring fips issue
names(industry_occupation)

digit5 <- function(data) {
  lapply(data$fips[data$fips%>%complete.cases], FUN = function(x) str_length(x)==5)
}

digit5(chemicals) %>% unlist() %>% sum() #number of fips with 5 digits
index <- digit5(chemicals) %>% unlist()
chemicals$fips[index] # so most of the fips are 5 digits

# merge data
merged <- merge(water_usage, industry_occupation, by = c('fips', 'year'), all=TRUE)
merged <- merge(merged, water_usage, by='fips', all=TRUE)

# exploring chemicals

# number of contaminated cities
chemicals[contaminant_level=='Greater than MCL', .N] #13545

# create a flag for every fips if there is a city that has contaminant_level greater than MCL
chemicals[, contaminated := as.integer(any(contaminant_level=='Greater than MCL')), by=c('fips', 'year')]

# number of counties with contaminated at least one city
chemicals[contaminated == 1L, .(uniqueN(fips))]

# extract the counties by year
years <- chemicals[contaminated == 1L, .(sort(unique(year)))]

# assign a contaminated regions population
chemicals[contaminant_level=='Greater than MCL', populated_affected := sum(pop_served), by=c('fips', 'year')]

chemicals[contaminant_level=='Greater than MCL', cities_contanimanted := .N, by=c('fips', 'year')]

# population affected for each region in each year
chemicals[complete.cases(populated_affected),unique(populated_affected), by=c('fips', 'year')]
population_affected <- chemicals[complete.cases(populated_affected),unique(populated_affected), by=c('fips', 'year')]
population_affected[, fips := as.character(fips)]
population_affected[str_length(population_affected$fips)==4, fips := str_pad(fips,5,'left','0')]
write_csv(population_affected,"Desktop/dataopen/population_affected.csv")

# cities affected for each region in each year
chemicals[complete.cases(populated_affected),unique(cities_contanimanted), by=c('fips', 'year')]
city_affected <- chemicals[complete.cases(cities_contanimanted),unique(cities_contanimanted), by=c('fips', 'year')]
city_affected[, fips := as.character(fips)]
city_affected[str_length(city_affected$fips) == 4, fips:= str_pad(fips,5,'left', '0')]
write_csv(city_affected,"Desktop/dataopen/city_affected.csv")

# create time series for population affected
population_affected_ts <- dcast(population_affected, fips ~year, value.var=c('V1'))
population_affected_ts <- ts(population_affected_ts)

# industry_occupation and earnings to see industry size
summary(industry_occupation)
summary(earnings)
names(industry_occupation) %in% names(earnings) %>% sum()
names(industry_occupation)
names(earnings)

industry_occupation_sub <- industry_occupation[,.(fips, agriculture, construction, manufacturing, wholesale_trade,
                       retail_trade, transport_utilities, information, finance_insurance_realestate,
                       prof_scientific_waste, edu_health, arts_recreation, public_admin,
                       other)]
setnames(x = industry_occupation_sub, old=names(industry_occupation_sub), new = c('fips1', 'agriculture1', 'construction1', 'manufacturing1', 'wholesale_trade1',
                                                                                  'retail_trade1', 'transport_utilities1', 'information1', 'finance_insurance_realestate1',
                                                                                  'prof_scientific_waste1', 'edu_health1', 'arts_recreation1', 'public_admin1',
                                                                                  'other1'))

earnings_sub <- earnings[,.(fips, agriculture=agri_fish_hunt, construction, manufacturing, wholesale_trade,
                        retail_trade, transport_warehouse_utilities, information, finance_insurance_realestate=fin_ins_realest,
                        prof_scientific_waste=prof_sci_tech, edu_health=total_edu_health_social,
                        arts_recreation=arts_ent_rec, public_admin=pub_admin, other=other_ser, year)]

industry_occupation_sub$fips %>%uniqueN()
earnings_sub$fips %>% uniqueN()

industry_merged <- merge(industry_occupation_sub, earnings_sub, by='fips', all=TRUE)

write_csv(earnings_sub, "Desktop/dataopen/earnings_sub.csv")



# chem
chem_val <- chemicals[, sum(value), by=c('fips', 'year')]
chem_val_ts <- chemicals[, sum(value), by=c('fips', 'year')] %>% dcast(fips ~ year, value.var='V1')
chem_val[, fips := as.character(fips)]
chem_val[str_length(chem_val$fips)==4, fips := str_pad(fips,5,'left','0')]
write_csv(chem_val, "Desktop/dataopen/chem_val.csv")

manufacturing_ts <- dcast(earnings, fips ~ year, value.var = 'manufacturing')



