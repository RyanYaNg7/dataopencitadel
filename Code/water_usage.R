

water_usage <- read.csv("water_usage.csv")


barplot(water_usage$state_fips,water_usage$total_withdrawal_3)

summary(water_usage$total_withdrawal_3)
is.na(water_usage$total_withdrawal_3)

#water_usage[!complete.cases(water_usage),]


total_usage_per_state <- aggregate(water_usage$total_withdrawal_3, by=list(Category=water_usage$state), FUN=sum)
barplot(total_usage_per_state$x)


total_population_per_state <- aggregate(water_usage$population, by=list(Category=water_usage$state), FUN=sum)
barplot(total_population_per_state$x)

usage_population <- merge(total_population_per_state,total_usage_per_state,by="Category")

names(usage_population) <- c("states","population","total_water_usage")

total_population_per_state <- aggregate(water_usage$population, by=list(Category=water_usage$state), FUN=sum)


#######################  drought data ##################

drought_data <- read.csv("droughts.csv")

drought_data_2010=drought_data[drought_data$year=="2010",]

drought_data_d4=drought_data_2010[drought_data_2010$d4!=0,]

drought_data_healthy=drought_data_2010[drought_data_2010$none==100,]
drought_data_not_healthy=drought_data_2010[drought_data_2010$none!=100,]

#### counties which were badly affected by droughts in 2010 were Hawaii Country and Maui County

################# check for above counties why there was a drought #############

water_usage_Hawaii_Maui =  water_usage[water_usage$county=="Hawaii County" | water_usage$county=="Maui County",]


################# hawaii county ##################
water_usage_Hawaii =  water_usage[water_usage$county=="Hawaii County",]
irrigation_Hawaii = water_usage_Hawaii$irrigation_3;
thermo_Hawaii = water_usage_Hawaii$thermoelectric_9
mining_Hawaii = water_usage_Hawaii$mining_9
livestock_Hawaii =  water_usage_Hawaii$livestock_3


slices <- c(irrigation_Hawaii,thermo_Hawaii,mining_Hawaii,livestock_Hawaii)
lbls <- c("Irrigation", "thermo", "mining", "livestock")
pie(slices, labels = lbls, main="Pie Chart of Hawai water consumption")


############## maui county #########################

water_usage_Maui =  water_usage[water_usage$county=="Maui County",]
irrigation_Maui = water_usage_Maui$irrigation_3;
thermo_Maui = water_usage_Maui$thermoelectric_9
mining_Maui = water_usage_Maui$mining_9
livestock_Maui =  water_usage_Maui$livestock_3


slices <- c(irrigation_Maui,thermo_Maui,mining_Maui,livestock_Maui)
lbls <- c("Irrigation", "thermo", "mining", "livestock")
pie(slices, labels = lbls, main="Pie Chart of Maui water consumption")

######################## for other counties #######################

#water_usage_all =  water_usage[water_usage$county=="Maui County",]
irrigation_all = mean(water_usage$irrigation_3);
thermo_all = mean(water_usage$thermoelectric_9)
mining_all = mean(water_usage$mining_9)
livestock_all =  mean(water_usage$livestock_3)


slices <- c(irrigation_all,thermo_all,mining_all,livestock_all)
lbls <- c("Irrigation", "thermo", "mining", "livestock")
pie(slices, labels = lbls, main="Pie Chart of all counties water consumption")


################# per capita usage of water in each state ########################

per_capita_usage = usage_population$total_water_usage/usage_population$population

barplot(per_capita_usage,names.arg = usage_population$states)


## name all states with exceptionally high per capita usage 

############################## education pattern #################

education <- read.csv("education_attainment.csv")

education$population_per_county <- education$less_than_hs + education$hs_diploma + education$some_college_or_associates + education$college_bachelors_or_higher
education <- education[-1,]

education_2012 <- education[education$year=="2012-2016",]

total_population_per_state_education <- aggregate(education_2012$population_per_county, by=list(states=education_2012$state), FUN=sum)
total_uneducated_per_state_education <- aggregate(education_2012$less_than_hs,by=list(states=education_2012$state),FUN=sum)


population_uneducated_2012 <- merge(total_population_per_state_education,total_uneducated_per_state_education,by="states")

usage_population$per_capita_usage <- usage_population$total_water_usage/usage_population$population

capita_education <- merge(usage_population,population_uneducated_2012,by="states")


capita_education$less_educated <- capita_education$x.y/capita_education$x.x
plot(capita_education$per_capita_usage,capita_education$less_educated)

#### states with high percentange of less educated people tends to consumer more water on terms of per capita usage

### advanced plot 

plot(capita_education$less_educated ~capita_education$per_capita_usage, col="lightblue", pch=19, cex=2)

mod <- lm(capita_education$less_educated~capita_education$per_capita_usage)
abline(mod, col="red", lwd=3)


### this add the labels to the points using their rownames
### font = 2 is bold

text(capita_education$per_capita_usage~capita_education$less_educated, labels=rownames(capita_education), cex=0.9, font=2)


### linear model for predicting water usage based on current hs education of US

hs_lm <- lm(per_capita_usage~less_educated,data=capita_education)

summary(hs_lm)
new_df <- data.frame("less_educated"=0.19)
prediction_usage <- predict(hs_lm,new_df)


###########################
library(openxlsx)
write.xlsx(usage_population,"usage_population_nigrah.xlsx")
write.xlsx(drought_data_d4,"drought_data_d4_nigrah.xlsx")
