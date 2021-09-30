OxCGRT_data <- read.csv("OxCGRT_latest_combined.txt")
NLD_data <- subset(OxCGRT_data, OxCGRT_data$CountryName =="Netherlands")                   
GER_data <- subset(OxCGRT_data, OxCGRT_data$CountryName =="Germany") # Assumptions; Country-wide policy measures were assumed for NRW specifically.

library(dplyr)
library(ggplot2)

# Select the policies/variables to be included in the model
NLD_data <- NLD_data %>%
  select(CountryName, CountryCode, Date, C1_combined_numeric, C2_combined_numeric,
         C3_combined_numeric, C4_combined_numeric, C8_combined_numeric, H6_combined_numeric,
         ConfirmedCases, ContainmentHealthIndex)

GER_data <- GER_data %>% 
  select(CountryName, CountryCode, Date, C1_combined_numeric, C2_combined_numeric,
         C3_combined_numeric, C4_combined_numeric, C8_combined_numeric, H6_combined_numeric,
         ConfirmedCases, ContainmentHealthIndex)

# Narrow-down space to March 2020 - Januari 2021
NLD_data <- subset(NLD_data, 20200301 <= NLD_data$Date)
NLD_data <- subset(NLD_data, NLD_data$Date <= 20201231)
NLD_data <- NLD_data[complete.cases(NLD_data),]
NLD_data$ConfirmedCases <- c(0,diff(NLD_data$ConfirmedCases))
NLD_data$Date <- as.Date(NLD_data$Date)
                         

GER_data <- subset(GER_data, 20200301 <= GER_data$Date)
GER_data <- subset(GER_data, GER_data$Date <= 20201231)
GER_data <- GER_data[complete.cases(GER_data),]

NRW_data <- read.csv("subset.csv")
str(NRW_data)
NRW_data$Date <- as.Date.character(NRW_data$Date)
str(NRW_data)

NRW_data$Date <- gsub("-", "", NRW_data$Date)
NRW_data <- subset(NRW_data, 20200301 <= NRW_data$Date)
NRW_data <- subset(NRW_data, NRW_data$Date <= 20201231)
NRW_data <- NRW_data[complete.cases(NRW_data),]

GER_data$ConfirmedCases <- NRW_data$Number_of_infections
GER_data$CountryName <- "North Rhine Westphalia"
GER_data$CountryCode <- "NRW"

# Finishing up...
NRW_data <- GER_data
names(NRW_data) <- gsub(x = names(NRW_data), pattern = "_combined_numeric", replacement = "")  
names(NLD_data) <- gsub(x = names(NLD_data), pattern = "_combined_numeric", replacement = "") 
library(dplyr)
Pdata <- full_join(NRW_data, NLD_data)

ggplot(Pdata, aes(x = Date, y = ConfirmedCases, colour = CountryCode)) + geom_area() 

