###
# This file is for Extracting data from https://www.data.gov.in using api.
###

# install.packages("devtools")
# devtools::install_github("econabhishek/datagovindia")

# load library and environment variables
readRenviron("Code/.Renviron")
library(datagovindia)

# Using my api Key
register_api_key("579b464db66ec23bdd0000011ef00582546f4074424e3a61b00a525a")
register_api_key(Sys.getenv("API_KEY"))


# To check out the available fields on the data
# get_api_fields("c2854035-995a-495f-ba31-c72e1dc49363")

####
# Major indicators State wise data
# link= https://www.data.gov.in/resource/major-socio-economic-indicators-2011-01082014-state-wise-ranking#api
StateIndicators = get_api_data(api_index =  "c2854035-995a-495f-ba31-c72e1dc49363")
# save data locally in RData formatẽ
# New column names

# we dont have data for telengana, as it was later divided.
new_column_names <- c(
   "Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar",
   "Chhattisgarh", "Delhi", "Goa", "Gujarat", "Haryana", "Himachal Pradesh", 
   "Jammu and Kashmir","Jharkhand", "Karnataka", "Kerala", "Madhya Pradesh",
  "Maharashtra", "Manipur", "Meghalaya", "Mizoram", "Nagaland",
  "Odisha", "Punjab", "Rajasthan", "Sikkim", "Tamil Nadu", "Tripura", 
  "Uttar Pradesh", "Uttarakhand", "West Bengal"
)
# change it according to state names in india
names(StateIndicators)[5:33] = new_column_names
StateIndicators <- StateIndicators[-c(42:82),]
save(StateIndicators, file = "Data/allStateIndicatorsData.Rdata")
####

####
# Crime Type and distribution
# link = https://www.data.gov.in/catalog/crime-committed-against-women
CrimeAgainstWomen = get_api_data(api_index = "8a801c01-ed02-4d3f-9d64-b54a2bbfd151")
# getting rid of extra sub fileds in crimes
CrimeAgainstWomen <- CrimeAgainstWomen[-c(2, 3, 4, 5, 6 ,7,
                                          10, 11, 12, 13, 14,
                                          17, 18, 19, 20, 21,
                                          23, 24,25,26,
                                          35, 36, 37,38,39,
                                          41:80
                                         ), ]
# renaming the names
crime_head = c(
  "Rape Cases",
  "Attempt to Rape",
  "Kidnapping & Abduction",
  "Dowry Deaths",
  "Assault outrage Modesty",
  "Insult of Modesty",
  "Cruelty By Family",
  "Girl Imports",
  "Abetment Suicides",
  "Dowry",
  "Indecent Representation",
  "Sati Prevention",
  "Domestic Violence",
  "Trafficking",
  "Total"
)
CrimeAgainstWomen$crime_head <- crime_head
CrimeAgainstWomen$X_2014 <- as.numeric(CrimeAgainstWomen$X_2014)
# save data locally in RData formatẽ
save(CrimeAgainstWomen, file = "Data/crimeAgainstWomenDistribution.Rdata")
####

####
# Abortion Cases stateWise
# link = https://www.data.gov.in/resource/statesuts-wise-health-management-information-system-hmis-data-abortion-cases-during-2021
AbortionData = get_api_data(api_index = "cd04062b-d5da-4b31-8b8e-7686900bd330")
# save data locally in RData formatẽ
save(AbortionData, file = "Data/abortionData.Rdata")
####

####
# Working Poputlation stateWise
# link = https://www.data.gov.in/resource/working-population-according-2011-census-states
WorkingPopulataion = get_api_data(api_index = "64d4bcb5-945d-48e8-84ec-995558785336")
# save data locally in RData formatẽ
save(WorkingPopulataion, file = "Data/workingPopulataion.Rdata")
####


####
# Population per policy
# link = https://www.data.gov.in/resource/working-population-according-2011-census-states
PolicePopulation = get_api_data(api_index = "51f098e6-f3a8-43c8-92e2-1252de6ec9ea")
# save data locally in RData formatẽ
save(PolicePopulation, file = "Data/policePopulation.Rdata")
####


