## Installation
```
install.packages("tidyverse")
install.packages("rvest")
install.packages("dplyr")
install.packages("devtools")
install.packages("tibble")
install.packages("viridis")
install.packages("reshape2")

# For installing the 'datagovindia' package from GitHub
devtools::install_github("econabhishek/datagovindia")
```

## Files

1. **CrimeDataExtractor.R**  
   This file is responsible for scraping and extracting crime data from Wikipedia, focusing on crime rates and related indicators for different states in India.

   - **Data Source**: [Wikipedia - List of states and union territories of India by crime rate](https://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_crime_rate)
   - **Output**: Extracted data is saved as `allStateCrimeData.Rdata` and `factorsAffectingCrime.Rdata`.

2. **differentIndicatorData.R**  
   This script fetches state-wise major socio-economic indicators using an API from the data.gov.in portal.

   - **Data Source**: [Major Socio-Economic Indicators - State-wise ranking](https://www.data.gov.in/resource/major-socio-economic-indicators-2011-01082014-state-wise-ranking#api)
   - **Output**: Saves state-wise indicators as `allStateIndicatorsData.Rdata`.

3. **politicalRatioData.R**  
   This script scrapes and extracts data related to political representation, focusing on the participation of women in various legislative bodies in India.

   - **Data Source**: [Press Information Bureau - Women representation data](https://pib.gov.in/PressReleasePage.aspx?PRID=1809217)
   - **Output**: Saves data in files like `lastFiveYearElection.Rdata`, `rsPercentage.Rdata`, `stateWisePoliticalData.Rdata`, and `stateWisePDataRecent.Rdata`.

4. **DataCleaningAndMerging.R**  
   This script cleans and merges multiple datasets, including data on police population, abortion cases, and other socio-economic indicators for each state.

   - **Data Sources**:
     - `abortionData.Rdata`: Data on state-wise abortion cases.
     - `policePopulation.Rdata`: Population per police data for states.
     - `factorsAffectingCrime.Rdata`: Factors influencing crime rates.
   - **Output**: Merged data saved as `combinedCrime.Rdata`.

## Data Sources

- **Wikipedia** - Crime rate data across states in India.
- **data.gov.in API** - Major socio-economic indicators, crime data against women, abortion data, and working population statistics.
- **Press Information Bureau (PIB)** - Data on women’s political representation in India.
