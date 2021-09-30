#-------------------------------------------------------------------------
# BM01BAM: Tutorial 4: Panel Data
#-------------------------------------------------------------------------

#remove(list=ls())

#-------------------------------------------------------------------------
# Load libraries
#-------------------------------------------------------------------------

# Package plyr for group statistics
# install.packages("plyr", dependencies = TRUE)
library(plyr)

# Package stargazer for appropriately formatted tables
# install.packages("stargazer", dependencies = TRUE)
library(stargazer)

# Package plm for panel estimation
# install.packages("plm", dependencies = TRUE)
library(plm)

# Package ggplot2 for graphics
# install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)


#-------------------------------------------------------------------------
# Construct a (balanced) panel data set, and make a datset with 
# group averages
#-------------------------------------------------------------------------

# Determine country averages of the included variables, as well
# as the number of non missing observations during the 
# selected observation period
Pdata.avg <- 
   ddply(Pdata, .(CountryName), summarise,
         avg.C1 = mean(C1, na.rm = TRUE),
         avg.C2 = mean(C2, na.rm = TRUE),
         avg.C3 = mean(C3, na.rm = TRUE),
         avg.C4 = mean(C4, na.rm = TRUE),
         avg.C8 = mean(C8, na.rm = TRUE),
         avg.H6 = mean(H6, na.rm = TRUE),
         avg.ConfirmedCases = mean(ConfirmedCases, na.rm = TRUE),
         avg.ContainmentHealthIndex = mean(ContainmentHealthIndex, na.rm = TRUE),
         numValid = length(CountryName))
         

# Merge averages in dfWorld.avg with Pdata_total (this can be done
# with 'mutate', but then the concise data frmae with country
# average will not be made available
Pdata_total <- merge(Pdata, Pdata.avg, by="CountryName")

# De-Mean the data..
attach(Pdata_total)
Pdata_total$diff.C1 <- C1 - avg.C1
Pdata_total$diff.C2 <- C2 - avg.C2
Pdata_total$diff.C3 <- C3 - avg.C3
Pdata_total$diff.C4 <- C4 - avg.C4
Pdata_total$diff.C8 <- C8 - avg.C8
Pdata_total$diff.H6 <- H6 - avg.H6
Pdata_total$diff.ConfirmedCases <- ConfirmedCases - avg.ConfirmedCases
Pdata_total$diff.ContainmentHealthIndex <- ContainmentHealthIndex - avg.ContainmentHealthIndex
detach(Pdata_total)


# renaming some columns for clarity
# names(Pdata_total) <- gsub(x = names(Pdata_total), pattern = "", replacement = " ")  
# names(Pdata_total) <- gsub(x = names(Pdata_total), pattern = "", replacement = " ")  
# str(Pdata_total) # for overview
#-------------------------------------------------------------------------
# Pooled regression model
#-------------------------------------------------------------------------

#------------------
# Formulate the model (very ad hoc)
#------------------
mdlA <- ConfirmedCases ~ C1 + C2 + C3 + C4 + C8 + H6

#------------------
# Make between and within group data frames
#------------------
# For convenience two datasets are made that contain the model
# variables for the within group differences and the between
# group difference

# ... find the variables of interest
mdlVars      <- all.vars(mdlA)
mdlVars.avg  <- paste0("avg.", mdlVars)
mdlVars.diff <- paste0("diff.", mdlVars)

# ... select variables from the data frames
Pdata_total.between <- Pdata.avg[mdlVars.avg]
Pdata_total.within  <- Pdata_total[mdlVars.diff]

# ... rename column names in order to make use of the 
# ... same model specification mdlA, and to conveniently
# ... merge the regresssion objects in stargazer
colnames(Pdata_total.within) <- 
   gsub("diff.", "", colnames(Pdata_total.within))
colnames(Pdata_total.between) <- 
   gsub("avg.", "", colnames(Pdata_total.between))



#------------------
# Estimation of the pooled model
#------------------
rsltPool <- lm(mdlA, data=Pdata_total)

summary(rsltPool)

stargazer(rsltPool,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE, type = "text")


# Pdata <- unique(Pdata, by = c("id", "time"))
table(index(Pdata_total), useNA = "ifany")

# Equivalently, using plm with option 'model="pooling"'
rslt.Pooling <- plm(mdlA, data = Pdata_total, model = "pooling")
summary(rslt.Pooling)

#------------------
# Estimation of the within and between group models
#------------------
rsltWithin <- lm (mdlA, data=Pdata_total.within)
summary(rsltWithin)

rsltBetween <- lm (mdlA, data=Pdata_total.between)
summary(rsltBetween)

stargazer(rsltWithin, rsltBetween,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE, type="text")



#-------------------------------------------------------------------------
# Fixed effect regression model
#-------------------------------------------------------------------------

# Functionality of the plm package more or less assumed that
# Country (the i dimension) and Year (the t dimension) are in
# the first two columns of the data frame (and iin this order).
# Parameter 'index' makes the identifying explicit (just in case
# they are in different positions)


# Estimate fixed effect model ('within'). Present specification
# estimates fixed country effects. A fixed year effects model
# can be estimated by adding option: effect="times". A fixed
# country-year effects model can be estimated by adding:
# effect="twoways"
rsltFE.Country <-
   plm(mdlA, data = Pdata_total,
       index=c("CountryCode", "Date"), model = "within")

# Tabulate the results
summary(rsltFE.Country)
stargazer(rsltPool, rsltFE.Country, rsltWithin,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE, type="text")

# Explore the estimated intercepts
summary(fixef(rsltFE.Country, type="dmean"))

# Compare the R2 from an LSDV model (with explicit specifi-
# cation of the year dummy variables)
summary(lm(update(mdlA, . ~ . + factor(CountryCode)), data=Pdata_total))

# Calculate for the fixed country effect model
R2.LSDV <- 1 - (var(residuals(rsltFE.Country))/
                   var(rsltFE.Country$model$H6))
R2.LSDV


# Evaluate the fixed effects model versus the pooled
# regression model
pFtest(rsltFE.Country, rsltPool)
pooltest(rslt.Pooling, rsltFE.Country)



#-------------------------------------------------------------------------
# Random effect regression model
#-------------------------------------------------------------------------

# Estimate random effect model ('random')
rsltRE.Country <-
   plm(mdlA, data = Pdata_total,
       index=c("CountryCode", "Date"), model = "random", random.method = "walhus")

# Tabulate the results
summary(rsltRE.Country)
stargazer(rsltPool, rsltFE.Country, rsltRE.Country,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE, type="text")

# Hausman test: compare random and fixed effects models.
# Under H0, no correlation between disturbance and explana-
# tory variables, both RE and FE are consistent (though FE
# is not efficient), under H1, correlation between distur-
# bance, only FE consistent
phtest(rsltFE.Country, rsltRE.Country)

# Extract results from object rsltRE.Country
# str(rsltRE.Country)

nCountry <- length(levels(attr(rsltRE.Country$residuals, "index")$Country))
nYear    <- length(levels(attr(rsltRE.Country$residuals, "index")$Year))
df.ehat  <- rsltRE.Country$df.residual
rmse     <- sqrt(sum(residuals(rsltRE.Country)^2)/df.ehat)

sig2.mu  <- unname(rsltRE.Country$ercomp$sigma2["id"])
sig2.eps <- unname(rsltRE.Country$ercomp$sigma2["idios"])
theta    <- unname(rsltRE.Country$ercomp$theta)
lambda   <- sig2.eps/(sig2.eps + nYear*sig2.mu)

round(cbind(sig2.mu, sig2.eps, theta, df.ehat, rmse, lambda), 3)

# Hausman test: compare random and fixed effects models.
# Under H0, no correlation between disturbance and explana-
# tory variables, both RE and FE are consistent (though FE
# is not efficient), under H1, correlation between distur-
# bance, only FE consistent
phtest(rsltFE.Country, rsltRE.Country)




#-------------------------------------------------------------------------
# First differences model regression model
#-------------------------------------------------------------------------

# Estimate random effect model ('random')
rsltFD.Country <-
   plm(mdlA, data = Pdata_total,
       index=c("Country", "Year"), model = "fd")

# Tabulate the results
summary(rsltFD.Country)
stargazer(rsltPool, rsltFE.Country, rsltRE.Country, rsltFD.Country,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE, type="text")

# Calculate rmse
df.ehat  <- rsltFD.Country$df.residual
rmse     <- sqrt(sum(residuals(rsltFD.Country)^2)/df.ehat)

round(cbind(df.ehat, rmse), 3)


#-------------------------------------------------------------------------
# Variable coefficients models
#-------------------------------------------------------------------------

# Fixed variable coefficients model, separate estimation
# per group
rsltVCFE.Country <- pvcm(mdlA, data = Pdata_total, model = "within")

# Random variable coefficients model, Swamy's GLS method
rsltVCRE.Country <- pvcm(mdlA, data = Pdata_total, model = "random")

# Evaluate fixed effects versus variable coefficients
pooltest(rslt.Pooling, rsltVCFE.Country)
pooltest(rsltFE.Country, rsltVCFE.Country)


# Summarize the results; note that stargazer does not recognize
# the class of the object
summary(rsltVCFE.Country)
summary(rsltVCRE.Country)


# Find fixed variable coefficients
coefficients(rsltVCFE.Country)

round(cbind(VCFE.avg =sapply(coefficients(rsltVCFE.Country), mean, na.rm = TRUE),
            VCFE.std =sapply(coefficients(rsltVCFE.Country), sd, na.rm = TRUE)),3)


# Find random variable coefficients
coefficients(rsltVCRE.Country)

# Display estimated effects
tmp <- coefficients(rsltFE.Country)

ggplot(tmp, aes(x=ContainmentHealthIndex)) +
   geom_histogram(fill="purple", colour="black") +
   theme(axis.title = element_text(size=rel(1.1)),
         axis.text = element_text(size=rel(1.1)))
#ggsave(paste0(dirRslt, "Session04VCFE_GDPkcap.pdf"))

ggplot(tmp, aes(x=pctAgriLnd)) +
   geom_histogram(fill="purple", colour="black") +
   theme(axis.title = element_text(size=rel(1.1)),
         axis.text = element_text(size=rel(1.1)))
#ggsave(paste0(dirRslt, "Session04VCFE_pctAgriLnd.pdf"))

