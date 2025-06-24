#This file is to extract data about crime rates


#load library
library(tidyverse)
library(rvest)
library(dplyr)


#read the html data
html=read_html("https://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_crime_rate")


#extracting tables from webpage
tabless=html%>%html_table()
tabless1=tabless[[3]]%>%as.data.frame()
tabless2=tabless[[4]];tabless2%>%as.data.frame()


#data cleaning
colnames(tabless1)=c('State','IPC.18','IPC.19','IPC.20','IPC.21','Percetage_share.18','percentage_share.19','Crime_rate.18','Crime_rate.19','Crime_rate.20','Crime_rate.21','Crime_Density','Investigation_Rate',"Charge.Sheeting_rate",'Conviction_Rate')
State_crime=tabless1[-c(1,2,3,32),]%>%as_tibble();State_crime
save(State_crime,file="Data/allStateCrimeData.Rdata")


# Crime rate (per 100,000 population) head-wise 2021.[2] 
# Crimes against children rate is calculated per 100,000 children population.

Crime_table=tabless2[-c(1,2,3,32),]%>%as_tibble();Crime_table
save(Crime_table,file="Data/factorsAffectingCrime.Rdata")