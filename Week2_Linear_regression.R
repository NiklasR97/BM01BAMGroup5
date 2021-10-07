# Empty the memory
remove(list=ls())
cat("\f")

#####Define folder structure#####
dir <- "C:/Users/zedek/Documents/RSM/Block1/BM01BAM - Advanced Statistics and Programming/BM01BAM_Group/"
dirData <- paste0(dir, "Data/")
dirProg <- paste0(dir, "Programs/")
dirRslt <- paste0(dir, "Results/")

#####Data preparation#####
OxCGRT_data <- read.csv(file=paste0(dirData,"OxCGRT_latest_combined.txt"))
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

NRW_data <- read.csv(file=paste0(dirData,"subset.csv"))
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

# Account for incubation period
# Creating new dataframes with polices from 1/3/2020-30/11/2020 and cases from 15/3/2020-14/12/2020 

#NRW

NRW_data2 <- NRW_data
names(NRW_data2)[names(NRW_data2) == 'ConfirmedCases'] <- 'CasesToday'
NRW_data2 <- subset(NRW_data2, NRW_data2$Date <= 20201130)
NRW_CasesAfter14Days <- NRW_data[,c("ConfirmedCases","Date")]
NRW_CasesAfter14Days <- subset(NRW_CasesAfter14Days, 20200315 <= NRW_CasesAfter14Days$Date)
NRW_CasesAfter14Days <- subset(NRW_CasesAfter14Days,  NRW_CasesAfter14Days$Date <= 20201214 )
NRW_data2$CasesAfter14Days <- NRW_CasesAfter14Days$ConfirmedCases

#Same for NLD

NLD_data2 <- NLD_data
names(NLD_data2)[names(NLD_data2) == 'ConfirmedCases'] <- 'CasesToday'
NLD_data2 <- subset(NLD_data2, NLD_data2$Date <= 20201130)
NLD_CasesAfter14Days <- NLD_data[,c("ConfirmedCases","Date")]
NLD_CasesAfter14Days <- subset(NLD_CasesAfter14Days, 20200315 <= NLD_CasesAfter14Days$Date)
NLD_CasesAfter14Days <- subset(NLD_CasesAfter14Days,  NLD_CasesAfter14Days$Date <= 20201214 )
NLD_data2$CasesAfter14Days <- NLD_CasesAfter14Days$ConfirmedCases

##### Linear Regression #####
######Load packages######
library(ggplot2)
library(RColorBrewer)
library(plyr)
library(L1pack)
library(quantreg)
library(car)
library(sandwich)
library(multiwayvcov)
library(stargazer)
library(lm.beta)
library(psych)
library(AER)

#####Formulate the models#####
#models1: C3 cancel public events
mdl1NLD <- NLD_data2$CasesAfter14Days ~ NLD_data2$C3
mdl1NRW <- NRW_data2$CasesAfter14Days ~ NRW_data2$C3
# Estimate the models
rslt1NLD <- lm(mdl1NLD, data=NLD_data2)
rslt1NRW <- lm(mdl1NRW, data=NRW_data2)
# Summarize results
summary(rslt1NLD)
#Linear regression of NLD on C3: CasesAfter14Days = 885.5 + 971.6*C3
#significant results
summary(rslt1NRW)
#Linear regression of NRW on C3: CasesAfter14Days = 250.0 + 470.6*C3
#*insignificant results

#models2: C8 restrictions on international travels
mdl2NLD <- NLD_data2$CasesAfter14Days ~ NLD_data2$C8
mdl2NRW <- NRW_data2$CasesAfter14Days ~ NRW_data2$C8
# Estimate the models
rslt2NLD <- lm(mdl2NLD, data=NLD_data2)
rslt2NRW <- lm(mdl2NRW, data=NRW_data2)
# Summarize results
summary(rslt2NLD)
#Linear regression of NLD on C8: CasesAfter14Days = 539.3 + 599.1*C8
#insignificant result for alpha; significant result for beta
summary(rslt2NRW)
#Linear regression of NRW on C8: CasesAfter14Days = 2112.6 - 304.4*C8
#significant results

#models3: H6 face masks mandate
mdl3NLD <- NLD_data2$CasesAfter14Days ~ NLD_data2$H6
mdl3NRW <- NRW_data2$CasesAfter14Days ~ NRW_data2$H6
# Estimate the models
rslt3NLD <- lm(mdl3NLD, data=NLD_data2)
rslt3NRW <- lm(mdl3NRW, data=NRW_data2)
# Summarize results
summary(rslt3NLD)
#Linear regression of NLD on H6: CasesAfter14Days = 387.3+1457.5*H6
#insignificant result for alpha; significant result for beta
summary(rslt3NRW)
#Linear regression of NRW on H6: CasesAfter14Days = 621.2 + 317.8*H6
#significant results

#models4: all 3 HC&C
mdl4NLD <- NLD_data2$CasesAfter14Days ~ NLD_data2$C3 + NLD_data2$C8 + NLD_data2$H6
mdl4NRW <- NRW_data2$CasesAfter14Days ~ NRW_data2$C3 + NRW_data2$C8 + NRW_data2$H6
# Estimate the models
rslt4NLD <- lm(mdl4NLD, data=NLD_data2)
rslt4NRW <- lm(mdl4NRW, data=NRW_data2)
# Summarize results
summary(rslt4NLD)
#Linear regression of NLD on C3, C8 and H6: CasesAfter14Days = -215.7 + 1812.0*C3 - 970.2*C8 + 2101.4*H6
#insignificant result for alpha; significant results for beta
summary(rslt4NRW)
#Linear regression of NRW on C3, C8 and H6: CasesAfter14Days = -679.75 + 2136.09*C3 - 761.29*C8 + 27.75*H6
#insignificant result for alpha and beta3; significant results for beta 1 and beta 2

######Goodness-of-fit######
#NLD
stargazer(rslt1NLD,rslt2NLD,rslt3NLD,rslt4NLD, type="text")
#NLD's model adjusted R^2 of model4 is 0.407

#NRW
stargazer(rslt1NRW,rslt2NRW,rslt3NRW,rslt4NRW, type="text")
#NRW's model adjusted R^2 of model4 is only 0.045

######Normality test######
#NLD
caseNLD_hist <- ggplot(NLD_data2,aes(CasesAfter14Days))
caseNLD_hist + geom_histogram() + labs(title="NLD",x="Cases after 14 days",y="Frequency")
#the histogram shows that the NLD case distribution is right skewed
qq.plot.caseNLD_hist <- qplot(sample=NLD_data2$CasesAfter14Days,stat="qq",main="NLD QQ plot",xlab="Theoretical quantiles",ylab="Sample quantiles")
qq.plot.caseNLD_hist
#The Q-Q plot shows that the NLD distribution has heavy tails, especially on the left
shapiro.test(NLD_data2$CasesAfter14Days)
#given that p < 0.05, we reject the hypothesis that the NLD data is normally distributed

#NRW
caseNRW_hist <- ggplot(NRW_data2,aes(CasesAfter14Days))
caseNRW_hist + geom_histogram() + labs(title="NRW",x="Cases after 14 days",y="Frequency")
#the histogram shows that the case distribution is right skewed
qq.plot.caseNRW_hist <- qplot(sample=NRW_data2$CasesAfter14Days,stat="qq",main="NRW QQ plot",xlab="Theoretical quantiles",ylab="Sample quantiles")
qq.plot.caseNRW_hist
#The Q-Q plot shows that the NRW distribution has heavy tails, especially on the left
shapiro.test(NRW_data2$CasesAfter14Days)
#given that p < 0.05, we reject the hypothesis that the NLD data is normally distributed
#since assumption a6 normality is violated, the linear regression model may not be suitable for the data

######Testing for heteroskedasticity######
#NLD
par(mfrow=c(2,2))
plot(mdl4NLD)
#residuals are not randomly distributed around the predicted values of Y, thus the errors have heteroskedasticity
lmtest::bgtest(mdl4NLD)
#given that p < 0.05, we conclude that errors are heteroskedastic

#NRW
par(mfrow=c(2,2))
plot(mdl4NRW)
#residuals are not randomly distributed around the predicted values of Y, thus the errors have heteroskedasticity
lmtest::bgtest(mdl4NRW)
#given that p < 0.05, we conclude that errors are heteroskedastic
#since assumption a4 homoskedasticity is violated, the linear regression model may not be suitable for the data

######Multicollinearity######
#NLD
#Imperfect multicollinearity with C3
full_modelNLD<- lm(CasesAfter14Days ~ C3 + C8 + H6, data = NLD_data2)
vifNLD<-vif(full_modelNLD)
stargazer(vifNLD, type="text")
#none of the explanatory variables has vifscore > 5, so multicollinearity is not obvious

#NRW
#Imperfect multicollinearity with C3
full_modelNRW<- lm(CasesAfter14Days ~ C3 + C8 + H6, data = NRW_data2)
vifNRW<-vif(full_modelNRW)
stargazer(vifNRW, type="text")
#none of the explanatory variables has vifscore > 5, so multicollinearity is not problematic;
# however, the vifscore of C3 is close to 5