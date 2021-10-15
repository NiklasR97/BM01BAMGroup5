OxCGRT_data <- read.csv("OxCGRT_latest_combined.txt")


NLD_data <- subset(OxCGRT_data, OxCGRT_data$CountryName =="Netherlands")                   
GER_data <- subset(OxCGRT_data, OxCGRT_data$CountryName =="Germany") # Assumptions; Country-wide policy measures were assumed for NRW specifically.

library(dplyr)
library(ggplot2)

# Select the policies/variables to be included in the model
NLD_data <- NLD_data %>%
  select(CountryName, CountryCode, Date, C1_combined_numeric, C2_combined_numeric,
         C3_combined_numeric, C4_combined_numeric, C5_combined_numeric, C8_combined_numeric, H6_combined_numeric,
         ConfirmedCases, ContainmentHealthIndex)

GER_data <- GER_data %>% 
  select(CountryName, CountryCode, Date, C1_combined_numeric, C2_combined_numeric,
         C3_combined_numeric, C4_combined_numeric, C5_combined_numeric, C8_combined_numeric, H6_combined_numeric,
         ConfirmedCases, ContainmentHealthIndex)

# Narrow-down space to March 2020 - December 2020
NLD_data <- subset(NLD_data, 20200301 <= NLD_data$Date)
NLD_data <- subset(NLD_data, NLD_data$Date <= 20201231)
NLD_data <- NLD_data[complete.cases(NLD_data),]
NLD_data$ConfirmedCases <- c(0,diff(NLD_data$ConfirmedCases))


GER_data <- subset(GER_data, 20200301 <= GER_data$Date)
GER_data <- subset(GER_data, GER_data$Date <= 20201231)
GER_data <- GER_data[complete.cases(GER_data),]

NRW_data <- read.csv("subset.csv")
str(NRW_data)
NRW_data$Date <- as.Date.character(NRW_data$Date)
str(NRW_data)

# After discussion with Dominik on 5-10 we implemented some more focus in our analyses
NRW_data$C1 <- NRW_data$C2 <- NRW_data$C4 <- NRW_data$ContainmentHealthIndex <- NULL
stargazer(NRW_data, title = "Descriptive statistics NRW data")

NLD_data$C1 <- NLD_data$C2 <- NLD_data$C4 <- NLD_data$ContainmentHealthIndex <- NULL
stargazer(NLD_data, title = "Descriptive statistics NLD data")


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

# library("writexl")
# write_xlsx(merged.data,"C:/Users/Media/OneDrive - Erasmus University Rotterdam/R_courses/AdvancedStatistics_programming/Group_project/merged.data.xlsx")

NRW_data$sumCases <- sum(NRW_data$ConfirmedCases)
NLD_data$sumCases <- sum(NLD_data$ConfirmedCases)
library(dplyr)
merged.data <- full_join(NRW_data, NLD_data)

# timeframe <- Origin_date + months(10)

#pdf("ConfirmedCasesOverview.pdf")
ggplot(merged.data, aes(x = Date, y = ConfirmedCases, colour = CountryCode)) + 
  geom_line(size=1.1) + theme(text = element_text(size = 15)) + xlim(c(20200301,20201201))
#dev.off()

library(plyr)
cases.sum <- ddply(merged.data, .(CountryName), summarise, sum.cases= sum(ConfirmedCases, na.rm=TRUE)/1000)

#pdf("ConfirmedCasesSum.pdf")
ggplot(cases.sum, aes(x=CountryName, y = sum.cases, colour = CountryName)) +
  geom_bar(stat="identity") +
  labs(y = "Sum confirmed cases (x1000)") + 
  theme(text = element_text(size = 15))
#dev.off()



################################################################################
# Regression analyses
################################################################################

# Creating new dataframes with polices from 1/3/2020-30/11/2020 and cases from 15/3/2020-14/12/2020 

#NRW

NRW_data2 <- NRW_data
names(NRW_data2)[names(NRW_data2) == 'ConfirmedCases'] <- 'CasesToday'
NRW_data2 <- subset(NRW_data2, NRW_data2$Date <= 20201130)
NRW_CasesAfter14Days <- NRW_data[,c("ConfirmedCases","Date")]
NRW_CasesAfter14Days <- subset(NRW_CasesAfter14Days, 20200315 <= 
                                 NRW_CasesAfter14Days$Date)
NRW_CasesAfter14Days <- subset(NRW_CasesAfter14Days, 
                               NRW_CasesAfter14Days$Date <= 20201214 )
NRW_data2$CasesAfter14Days <- NRW_CasesAfter14Days$ConfirmedCases

#Same for NLD

NLD_data2 <- NLD_data
names(NLD_data2)[names(NLD_data2) == 'ConfirmedCases'] <- 'CasesToday'
NLD_data2 <- subset(NLD_data2, NLD_data2$Date <= 20201130)
NLD_CasesAfter14Days <- NLD_data[,c("ConfirmedCases","Date")]
NLD_CasesAfter14Days <- subset(NLD_CasesAfter14Days, 20200315 <= 
                                 NLD_CasesAfter14Days$Date)
NLD_CasesAfter14Days <- subset(NLD_CasesAfter14Days,  
                               NLD_CasesAfter14Days$Date <= 20201214 )
NLD_data2$CasesAfter14Days <- NLD_CasesAfter14Days$ConfirmedCases

#Define control variable - population density
#popdNLD <- rep(518,275)
#NLD_data2$popd <- popdNLD
#popdNRW <- rep(526,275)
#NRW_data2$popd <- popdNRW


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
library(dplyr)
library(tidyverse)
library(MASS)

# Add control variables for number of wave
# NLD
# Implementing Wave indicator variable (1, 2, and 3)
NLD_data2$dWave1 <- ifelse(NLD_data2$Date < 20200601, 1, 0)

NLD_data2$dWave2 <- ifelse(NLD_data2$Date >= 20200701 & 
                             NLD_data2$Date < 20201201, 1, 0)

# NRW
NRW_data2$dWave1 <- ifelse(NRW_data2$Date < 20200601, 1, 0)

NRW_data2$dWave2 <- ifelse(NRW_data2$Date >= 20200701 & 
                             NRW_data2$Date < 20201201, 1, 0)


# Models
mdl4NLD <- CasesAfter14Days ~ C3 + C8 + H6
mdl5NLD <- CasesAfter14Days ~ C3 + C8 + H6 + dWave1 +dWave2

rsltNegBinomNLD <- glm.nb(mdl4NLD, data= NLD_data2)

# Estimate the models
# NLD
rslt4NLD <- lm(mdl4NLD, data=NLD_data2)
rslt5NLD <- lm(mdl5NLD, data=NLD_data2)
rsltNegBinom4NLD <- glm.nb(mdl4NLD, data= NLD_data2)
rsltNegBinom5NLD <- glm.nb(mdl5NLD, data= NLD_data2)


# NRW
# Models
mdl4NRW <- CasesAfter14Days ~ C3 + C8 + H6
mdl5NRW <- CasesAfter14Days ~ C3 + C8 + H6 + dWave1 + dWave2

rsltNegBinomNRW <- glm.nb(mdl4NRW, data= NRW_data2)

# Estimate the models
# NRW
rslt4NRW <- lm(mdl4NRW, data=NRW_data2)
rslt5NRW <- lm(mdl5NRW, data=NRW_data2)
rsltNegBinom4NRW <- glm.nb(mdl4NRW, data= NRW_data2)
rsltNegBinom5NRW <- glm.nb(mdl5NRW, data= NRW_data2)

# table...
stargazer(rslt4NLD, rslt5NLD, rslt4NRW, rslt5NRW,  
          intercept.bottom=FALSE, no.space = TRUE, align = FALSE,
          font.size = "tiny")


# table...
stargazer(rsltNegBinom4NLD, rsltNegBinom5NLD, rsltNegBinom4NRW, rsltNegBinom5NRW,
          intercept.bottom=FALSE, no.space = TRUE, align = FALSE,
          font.size = "tiny")

#-------------------------------------------------------------------------------
# Endogeneity check
#-------------------------------------------------------------------------------

# Merging sets
merged.data2 <- full_join(NRW_data2, NLD_data2)

# Implementing country dummy
merged.data2$dCountry <- ifelse(merged.data2$CountryCode == "NRW", 1, 0) # dont forget offsetting infections/policy implementations

C3.mdlA <- C3 ~ dCountry + CasesAfter14Days
C3.mdlB <- C3 ~ dCountry + CasesAfter14Days + dCountry:CasesAfter14Days

C8.mdlA <- C8 ~ dCountry + CasesAfter14Days
C8.mdlB <- C8 ~ dCountry + CasesAfter14Days + dCountry:CasesAfter14Days  

H6.mdlA <- H6 ~ dCountry + CasesAfter14Days
H6.mdlB <- H6 ~ dCountry + CasesAfter14Days + dCountry:CasesAfter14Days


# Estimate the models...
C3.rsltOLsA <- lm(C3.mdlA, data=merged.data2) 
C3.rsltOLsB <- lm(C3.mdlB, data=merged.data2) 

C8.rsltOLsA <- lm(C8.mdlA, data=merged.data2)
C8.rsltOLsB <- lm(C8.mdlB, data=merged.data2)

H6.rsltOLsA <- lm(H6.mdlA, data=merged.data2)
H6.rsltOLsB <- lm(H6.mdlB, data=merged.data2)

# Make a stargazer table
stargazer(C3.rsltOLsA, C3.rsltOLsB, C8.rsltOLsA, C8.rsltOLsB, H6.rsltOLsA, H6.rsltOLsB,
          intercept.bottom=FALSE, no.space = TRUE, font.size="tiny", type = "text")

stargazer(NLD_data2, NRW_data2, 
          intercept.bottom=FALSE, no.space = TRUE, font.size="tiny")

#-------------------------------------------------------------------------------
# Lagged instrumental variable tests
#-------------------------------------------------------------------------------

# non-used columns
NLD_data2$C1<-NLD_data2$C2<-NLD_data2$C4<-NLD_data2$C5<-NULL

# 1-month lagged IV for C3
C3s <- NLD_data2$C3[1:244]
C3s <- c(rep(0, 31), C3s)
NLD_data2$C3MonthPrior <- C3s
NRW_data2$C3MonthPrior <- C3s

# 1-month lagged IV for C8
C8s <- NLD_data2$C8[1:244]
C8s <- c(rep(0, 31), C8s)
NLD_data2$C8MonthPrior <- C8s
NRW_data2$C8MonthPrior <- C8s

# 1-month lagged IV for H6
H6s <- NLD_data2$H6[1:244]
H6s <- c(rep(0, 31), H6s)
NLD_data2$H6MonthPrior <- H6s
NRW_data2$H6MonthPrior <- H6s



# new model with instrumental variables
mdl <- CasesAfter14Days ~ C3 + C8 + H6
mdlIV <- CasesAfter14Days ~ C3 + C8 + H6 | C3MonthPrior + C8MonthPrior + H6MonthPrior


# model estimations
rsltNLD <- lm(mdl, data=NLD_data2)
rsltNLDIV <- ivreg(mdlIV, data=NLD_data2)
rsltNRW <-lm(mdl, data=NRW_data2)
rsltNRWIV <-ivreg(mdlIV, data=NRW_data2)


stargazer(rsltNLD, rsltNLDIV, rsltNRW, rsltNRWIV, align = TRUE, 
          intercept.bottom = FALSE, no.space = TRUE, font.size = "tiny", type = "text")




#-------------------------------------------------------------------------------
# Count models
#-------------------------------------------------------------------------------


# Package MASS for the negative binomial regression 
library(MASS)
# PAckage sandwich to determine heteroskedaticity
# robust standard errors

# for the vcovHC function
library(sandwich)  
# Package extraDistr for random sampling from a
# conditional Poisson distribution
library(extraDistr)
library(stargazer)

# Model specification and estimation
rsltPoissonNLD <- glm(mdl4NLD, data = NLD_data2, family = c("poisson"))
rsltPoissonNRW <- glm(mdl4NRW, data = NRW_data2, family = c("poisson"))

rsltQuasiPoissonNLD <- glm(mdl4NLD, data = NLD_data2, family = c("quasipoisson"))
rsltQuasiPoissonNRW <- glm(mdl4NRW, data = NRW_data2, family = c("quasipoisson"))

rsltNegBinomNLD <- glm.nb(mdl4NLD, data= NLD_data2)
rsltNegBinomNRW <- glm.nb(mdl4NRW, data= NRW_data2)

seWhite <- sqrt(diag(vcovHC(rsltPoisson, type = "HC0")))


stargazer(rsltPoissonNLD, rsltPoissonNRW, rsltQuasiPoissonNLD, 
          rsltQuasiPoissonNRW, rsltNegBinomNLD, rsltNegBinomNRW, 
          no.space=TRUE, intercept.bottom = FALSE, font.size = "tiny",
          se = list(NULL, seWhite, NULL, NULL))




################################################################################
# DiD analysis
################################################################################



# I am creating new dataframes with polices from 1/3/2020-30/11/2020 and cases from 15/3/2020-14/12/2020 

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

#For each policy I will make a different data frame with both countries. The chosen dates are
#the period with the more strict policy for NRW
#I will create DiD plots


#C3 

DiD_C3_NRW <- NRW_data2[,c("CountryCode","Date","C3","CasesAfter14Days")]
DiD_C3_NLD <- NLD_data2[,c("CountryCode","Date","C3","CasesAfter14Days")]
DiD_C3_NRW <- subset(DiD_C3_NRW, DiD_C3_NRW$Date <= 20200801)
DiD_C3_NRW <- subset(DiD_C3_NRW,  20200601 <= DiD_C3_NRW$Date )
DiD_C3_NLD <- subset(DiD_C3_NLD, DiD_C3_NLD$Date <= 20200801)
DiD_C3_NLD <- subset(DiD_C3_NLD,  20200601 <= DiD_C3_NLD$Date )

# Fixing the dates with a pseudo-day indicator to get rid of the weird straight 
# lines due to the continuous dat variable.
DiD_C3 <- rbind(DiD_C3_NRW,DiD_C3_NLD)
D.Sequence <- 1:nrow(DiD_C3)
DiD_C3$DayIndicator <- c(D.Sequence[1:62],D.Sequence[1:62])

#Other useful dummies
DiD_C3$dPeriod <- c(rep(0,31),rep(1,31),rep(0,31),rep(1,31))
DiD_C3$dCountry <- ifelse(DiD_C3$CountryCode == "NRW", 1, 0)

# Define model
mdlC3 <- CasesAfter14Days ~ dCountry + dPeriod
mdlC3Int <- CasesAfter14Days ~ dCountry + dPeriod + dCountry:dPeriod

rsltC3 <- lm(mdlC3Int, data=DiD_C3, subset = (DiD_C3$CountryCode == "NRW")) 
rsltC3Int <- lm(mdlC3Int, data=DiD_C3)

pdf("DiD_C3.pdf")
# plotting C3
# ggplot(DiD_C3, aes(DayIndicator,CasesAfter14Days, color = CountryCode)) +
  labs(title = "Cancel Public Events [C3]") +
  stat_summary(geom = 'line') + 
  geom_vline(xintercept = 31) + 
  theme(text = element_text(size = 15))
# dev.off()

#C8

DiD_C8_NRW <- NRW_data2[,c("CountryCode","Date","C8","CasesAfter14Days")]
DiD_C8_NLD <- NLD_data2[,c("CountryCode","Date","C8","CasesAfter14Days")]
DiD_C8_NRW <- subset(DiD_C8_NRW, DiD_C8_NRW$Date <= 20200418)
DiD_C8_NLD <- subset(DiD_C8_NLD, DiD_C8_NLD$Date <= 20200418)

DiD_C8 <- rbind(DiD_C8_NRW,DiD_C8_NLD)

DiD_C8 <- rbind(DiD_C8_NRW,DiD_C8_NLD)
D.Sequence <- 1:nrow(DiD_C8)
DiD_C8$DayIndicator <- c(D.Sequence[1:49],D.Sequence[1:49])

#Other useful dummies
DiD_C8$dPeriod <- c(rep(0,24),rep(1,25),rep(0,24),rep(1,25))
DiD_C8$dCountry <- ifelse(DiD_C8$CountryCode == "NRW", 1, 0)

# Define model
mdlC8Int <- CasesAfter14Days ~ dCountry + dPeriod + dCountry:dPeriod

rsltC8 <- lm(mdlC8Int, data=DiD_C8, subset= (DiD_C8$CountryCode == "NRW")) 
rsltC8Int <- lm(mdlC8Int, data=DiD_C8)


# pdf("DiD_C8.pdf")
ggplot(DiD_C8, aes(DayIndicator,CasesAfter14Days, color = CountryCode)) +
  labs(title = "International Travel Controls [C8]") +
  stat_summary(geom = 'line') +
  geom_vline(xintercept = 25) +
  theme(text = element_text(size = 15))
# dev.off()

#H6

DiD_H6_NRW <- NRW_data2[,c("CountryCode","Date","H6","CasesAfter14Days")]
DiD_H6_NLD <- NLD_data2[,c("CountryCode","Date","H6","CasesAfter14Days")]
DiD_H6_NRW <- subset(DiD_H6_NRW, DiD_H6_NRW$Date <= 20200506)
DiD_H6_NRW <- subset(DiD_H6_NRW,  20200306 <= DiD_H6_NRW$Date )
DiD_H6_NLD <- subset(DiD_H6_NLD, DiD_H6_NLD$Date <= 20200506)
DiD_H6_NLD <- subset(DiD_H6_NLD,  20200306 <= DiD_H6_NLD$Date )

DiD_H6 <- rbind(DiD_H6_NRW,DiD_H6_NLD)

DiD_H6 <- rbind(DiD_H6_NRW,DiD_H6_NLD)
D.Sequence <- 1:nrow(DiD_H6)
DiD_H6$DayIndicator <- c(D.Sequence[1:62],D.Sequence[1:62])

#Other useful dummies
DiD_H6$dPeriod <- c(rep(0,31),rep(1,31),rep(0,31),rep(1,31))
DiD_H6$dCountry <- ifelse(DiD_H6$CountryCode == "NRW", 1, 0)

# Define model
mdlH6Int <- CasesAfter14Days ~ dCountry + dPeriod + dCountry:dPeriod

rsltH6 <- lm(mdlH6Int, data=DiD_H6, subset= (DiD_H6$CountryCode =="NRW")) 
rsltH6Int <- lm(mdlH6Int, data=DiD_H6)



# pdf("DiD_H6.pdf")
ggplot(DiD_H6, aes(DayIndicator,CasesAfter14Days, color = CountryCode)) +
  labs(title = "Facial Coverings") +
  stat_summary(geom = 'line') +
  geom_vline(xintercept = 31) +
  theme(text = element_text(size = 15))
# dev.off()  



#-------------------------------------------------------------------------------
# DiD regression analysis
#-------------------------------------------------------------------------------

stargazer(rsltC3, rsltC3Int, rsltC8, rsltC8Int, rsltH6, rsltH6Int, align = FALSE, 
          intercept.bottom = FALSE, no.space = TRUE, font.size = "tiny")

stargazer(rsltC3, rsltC3Int, intercept.bottom = FALSE, no.space = TRUE, 
          font.size = "tiny")

stargazer(rsltC8, rsltC8Int, intercept.bottom = FALSE, no.space = TRUE, 
          font.size = "tiny")

stargazer(rsltH6, rsltH6Int, intercept.bottom = FALSE, no.space = TRUE, 
          font.size = "tiny")

# ------------------------------------------------------------------------------
#Matrices
# ------------------------------------------------------------------------------

#C3

# Define models
mdlC3 <- CasesAfter14Days ~ C3 + Date + C3:Date

# Estimate the models
rsltOLC3 <- lm(mdlC3, data=DiD_C3)

DiD_C3$period <- ifelse( 20200701 <= DiD_C3$Date,"2","1")

library(plyr)

avg_C3 <- ddply(DiD_C3,.(period,CountryCode), summarise,
                avg_C3 = mean(CasesAfter14Days, na.rm=TRUE))

library(reshape2)

mtr_C3 <- dcast(avg_C3, period ~ CountryCode, value.var = "avg_C3")
correct_matrC3 <- mtr_C3[,c(1,3,2)]
mtr_C3 <- correct_matrC3
rm(correct_matrC3)
mtr_C3$period <- as.numeric(as.character(mtr_C3$period))
mtr_C3 <- rbind(mtr_C3, mtr_C3[2,] - mtr_C3[1,])

rownames(mtr_C3) <- c("Before", "After", "Difference")
mtr_C3[3,"period"] <- NA




#C8

# Define models
mdlC8 <- CasesAfter14Days ~ C8 + Date + C3:Date

# Estimate the models
rsltOLC8 <- lm(mdlC8, data=DiD_C8)

DiD_C8$period <- ifelse( 20200318 <= DiD_C8$Date,"2","1")

avg_C8 <- ddply(DiD_C8,.(period,CountryCode), summarise,
                avg_C8 = mean(CasesAfter14Days, na.rm=TRUE))

mtr_C8 <- dcast(avg_C8, period ~ CountryCode, value.var = "avg_C8")
correct_matrC8 <- mtr_C8[,c(1,3,2)]
mtr_C8 <- correct_matrC8
rm(correct_matrC8)
mtr_C8$period <- as.numeric(as.character(mtr_C8$period))
mtr_C8 <- rbind(mtr_C8, mtr_C8[2,] - mtr_C8[1,])

rownames(mtr_C8) <- c("Before", "After", "Difference")
mtr_C8[3,"period"] <- NA


#H6

# Define models
mdlH6 <- CasesAfter14Days ~ H6 + Date + C3:Date

# Estimate the models
rsltOLH6 <- lm(mdlH6, data=DiD_H6)

DiD_H6$period <- ifelse( 20200406 <= DiD_H6$Date,"2","1")

avg_H6 <- ddply(DiD_H6,.(period,CountryCode), summarise,
                avg_H6 = mean(CasesAfter14Days, na.rm=TRUE))

mtr_H6 <- dcast(avg_H6, period ~ CountryCode, value.var = "avg_H6")
correct_matrH6 <- mtr_H6[,c(1,3,2)]
mtr_H6 <- correct_matrH6
rm(correct_matrH6)
mtr_H6$period <- as.numeric(as.character(mtr_H6$period))
mtr_H6 <- rbind(mtr_H6, mtr_H6[2,] - mtr_H6[1,])

rownames(mtr_H6) <- c("Before", "After", "Difference")
mtr_H6[3,"period"] <- NA





################################################################################
# Normality, skedasticity, multicollinearity tests
################################################################################

######Goodness-of-fit######
#NLD's model adjusted R^2 of model4 is 0.40 
#NRW's model adjusted R^2 of model4 is only 0.045

######Normality test######
#NLD
caseNLD_hist <- ggplot(NLD_data2,aes(CasesAfter14Days))
caseNLD_hist + geom_histogram() + labs(title="Netherlands",x="Cases after 14 days",y="Frequency")
#the histogram shows that the NLD case distribution is right skewed
qq.plot.caseNLD_hist <- qplot(sample=NLD_data2$CasesAfter14Days,stat="qq",main="Netherlands QQ plot",xlab="Theoretical quantiles",ylab="Sample quantiles")
qq.plot.caseNLD_hist
#The Q-Q plot shows that the NLD distribution has heavy tails, especially on the left
shapiro.test(NLD_data2$CasesAfter14Days)
#given that p < 0.05, we reject the hypothesis that the NLD data is normally distributed

#NRW
caseNRW_hist <- ggplot(NRW_data2,aes(CasesAfter14Days))
caseNRW_hist + geom_histogram() + labs(title="North Rhine-Westphalia",x="Cases after 14 days",y="Frequency")
#the histogram shows that the case distribution is right skewed
qq.plot.caseNRW_hist <- qplot(sample=NRW_data2$CasesAfter14Days,stat="qq",main="North Rhine-Westphalia QQ plot",xlab="Theoretical quantiles",ylab="Sample quantiles")
qq.plot.caseNRW_hist
#The Q-Q plot shows that the NRW distribution has heavy tails, especially on the left
shapiro.test(NRW_data2$CasesAfter14Days)
#given that p < 0.05, we reject the hypothesis that the NLD data is normally distributed
#since assumption a6 normality is violated, the linear regression model may not be suitable for the data

######Testing for heteroskedasticity######
#NLD
par(mfrow=c(2,2))
plot(mdl4NLD, main = "NRW", ylab = "number of cases", data = NLD_data2)
#residuals are not randomly distributed around the predicted values of Y, thus the errors have heteroskedasticity
lmtest::bgtest(mdl4NLD, data = NLD_data2)
#given that p < 0.05, we conclude that errors are heteroskedastic

#NRW
par(mfrow=c(2,2))
plot(mdl4NRW, main = "Netherlands", ylab = "number of cases", data = NRW_data2)
#residuals are not randomly distributed around the predicted values of Y, thus the errors have heteroskedasticity
lmtest::bgtest(mdl4NRW, data = NRW_data2)
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
