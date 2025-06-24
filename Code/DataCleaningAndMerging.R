# abortion and miscarriage data
load("Data/abortionData.Rdata")
names(AbortionData) = c('States', 'cases')
# Adding Telengana and Andra pradesh
AbortionData$cases[2] = AbortionData$cases[31] + AbortionData$cases[2]
# Swaping for order
AbortionData[c(34, 35), ] = AbortionData[c(35, 34), ]
# Drop the rows of no use
AbortionData <- AbortionData[-c(1, 6, 17, 18, 26, 31, 32, 37:74), ]


# Police Data
load("Data/policePopulation.Rdata")
PolicePopulation <- PolicePopulation[1:28, ]
# Adding Telengana and Andra pradesh
PolicePopulation[1, 4] = (PolicePopulation[1, 3] + PolicePopulation[24, 3])/
  (PolicePopulation[1, 3]/PolicePopulation[1, 4] + PolicePopulation[24, 3]/PolicePopulation[24, 4])

PolicePopulation[1, 3] = PolicePopulation[1, 3] +PolicePopulation[24, 3]

PolicePopulation <- PolicePopulation[-c(24), -c(1)]
names(PolicePopulation)[1] = 'States'
#  Data dont contain delhi and J&K values.

# differents of crime and other factors data
load("Data/factorsAffectingCrime.Rdata")
Crime_table <- Crime_table[-c(28:30, 33:35), ]
names(Crime_table)[1] = 'States'

# Merging with NA for missing values
merged_df <- merge(Crime_table, PolicePopulation, by = 'States', all = TRUE)
merged_df <- merge(merged_df, AbortionData, by = 'States', all = TRUE)
# convert to numeric data
merged_df <-  lapply(merged_df[, -c(1)],as.numeric) %>% as.data.frame() %>% cbind(merged_df$States)
merged_df <- merged_df[, c(ncol(merged_df), 1:(ncol(merged_df) - 1))]
names(merged_df)[1] = "States"
# Replace NA with 0 in the numeric columns (excluding the first column)
merged_df[,-1][is.na(merged_df[,-1])] <- 0
# Adding Telengana and Andra pradesh
merged_df[1, -c(1)] <- merged_df[1, -c(1)] + merged_df[26, -c(1)]
merged_df <- merged_df[-c(26), -c(12)]
names(merged_df) <- c(
  "States",
  "Violent Crimes",
  "Murder",
  "Rape",
  "Kidnapping",
  "Crimes Against Children",
  "Extortion",
  "Robbery",
  "Hit and Run",
  "Drugs Trafficking",
  "Illegal Arms",
  "Women Population / Women Police",
  "Cases"
)

# # transpose of it
# name <- merged_df$States
# merged_df <- merged_df[, -c(1)] %>% t() %>% as.data.frame()
# names(merged_df) <- name
# combined_crime <- merged_df
# combined_crime <- rownames_to_column(combined_crime, var = "Indicator")

combined_crime <- merged_df
# check if success full?
load("Data/allStateIndicatorsData.Rdata")
if(all.equal(names(StateIndicators)[-c(1, 2, 3, 4)], combined_crime$States) == TRUE){
  save(combined_crime, file = "Data/combinedCrime.Rdata")
}



# clean election data
load("Data/rsPercentage.Rdata")
load("Data/lastFiveYearElection.Rdata")

lastFiveYearElection <- lastFiveYearElection[c(-1, -2), c(-2, -3, -5, -6, -8)] %>%
  rename(
    Year = X1,
    `Percentage Women Contestants` = X4,
    `Percentage Women Elected` = X7
  ) %>%
  mutate(
    `Percentage Women Elected` = as.numeric(`Percentage Women Elected`),
    `Percentage Women Contestants` = as.numeric(`Percentage Women Contestants`)
  )
rsPercentage <- rsPercentage %>%
  mutate(`Women Percentage` = as.numeric(`Women Percentage`)) %>%
  rename(`Percentage Women Elected` = `Women Percentage`,
         Year = year)
save(file = "Data/rsPercentage.Rdata", rsPercentage)
save(file = "Data/lastFiveYearElection.Rdata", lastFiveYearElection)

# elctioon data of each state
load("Data/stateWisePoliticalData.Rdata")


stateWisePoliticalData <- stateWisePoliticalData[ ,-1]
names(stateWisePoliticalData) <- stateWisePoliticalData[1,]
stateWisePoliticalData <- stateWisePoliticalData[-1, ] %>% 
  mutate(`% Of Women Contestants` = as.numeric(`% Of Women Contestants`),
         `% Of Seats won by Women` = as.numeric(`% Of Seats won by Women`))

save(file = "Data/stateWisePoliticalData.Rdata", stateWisePoliticalData)

# state incicators
load("Data/allStateIndicatorsData.Rdata")

StateIndicators <- StateIndicators %>%
  mutate(across(5:ncol(.), as.numeric))

save(file = "Data/allStateIndicatorsData.Rdata", StateIndicators)


load("Data/allStateIndicatorsData.Rdata")
write.csv(your_dataframe, "Data/allStateIndicatorsData.csv")
