library(rvest)
library(dplyr)
library(tibble)

web_link =read_html('https://pib.gov.in/PressReleasePage.aspx?PRID=1809217')

#The number and percentage of women Contestants 
# and Women elected over Total Seats in Last 5 General Elections to Lok Sabha
lastFiveYearElection = html_table(web_link) %>% .[[1]]
save(lastFiveYearElection, file = "Data/lastFiveYearElection.Rdata")
#representation of women nominated in rajya sabha
# during 2012 to 2021
table_2 = html_table(web_link) %>% .[[2]]
table_2$X1 = (substr(table_2$X1, start= 1, stop = 4))
rsPercentage =table_2[-c(1,2),]
colnames(rsPercentage) <- c("year", "women number","Women Percentage")
save(rsPercentage, file = "Data/rsPercentage.Rdata")

#participation of woman in legislative assemblies of state
stateWisePoliticalData = html_table(web_link) %>% .[[3]]
save(stateWisePoliticalData, file = "Data/stateWisePoliticalData.Rdata")


#percentage of women elected in recentliy
#concluded legislative assembly elections of 5states 2022
stateWisePDataRecent = html_table(web_link) %>% .[[4]]
save(stateWisePDataRecent, file = "Data/stateWisePDataRecent.Rdata")


#for all tables
table_ = html_table(web_link)
