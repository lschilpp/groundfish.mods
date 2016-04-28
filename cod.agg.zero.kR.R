#Liberty Schilpp
#Cod Data Models 
#12/8/15#rev 1/18/16, 1/22/16, 1/28/16, revised 2/11/16,3/22/2016
#This is for the models with all ages combined
#data manipulation section:
cod.data <- read.csv("~/Desktop/MS Thesis/cod_age_data.csv")
#add cohort 
cod.data$COHORT = cod.data$YEAR - cod.data$AGE
#to get only the weight data (post 1992)
cod.wt.data <- cod.data[cod.data$YEAR>=1992,]
#add logweight
cod.wt.data$logwt = log(cod.wt.data$INDWT)
#add condtion factor
cod.wt.data$K = 100000*(cod.wt.data$INDWT/(cod.wt.data$LENGTH^3))
#add log k
cod.wt.data$logk = log(cod.wt.data$K)
#add age zero variable
cod.wt.data$ZERO <- NA
cod.wt.data$ZERO[cod.wt.data$AGE >= 1] <- "N"
cod.wt.data$ZERO[cod.wt.data$AGE < 1] <- "Y"
head(cod.wt.data)
#location - Georges Bank = strata 1130 - 1250, GOM = 1260-1400
#Create a new column and fill with NAs
cod.wt.data$LOCATION <- NA
#Now assign the locations depending on the values for Stratum
cod.wt.data$LOCATION[cod.wt.data$STRATUM >= 1130 & cod.wt.data$STRATUM <= 1250] <- "GB"
cod.wt.data$LOCATION[cod.wt.data$STRATUM >= 1260 & cod.wt.data$STRATUM <= 1400] <- "GOM"
head(cod.wt.data);
#set season as a factor
cod.wt.data$SEASON=as.factor(cod.wt.data$SEASON);
#set location as a factor
cod.wt.data$LOCATION=as.factor(cod.wt.data$LOCATION);
#set agezero as a factor
cod.wt.data$ZERO=as.factor(cod.wt.data$ZERO);
#set year as a factor
cod.wt.data$YEAR=as.factor(cod.wt.data$YEAR);
#install.packages("lme4")
library(lme4)

cod.data.1 <- cod.wt.data;
cod.data.1 <- na.omit(cod.data.1)
#remove rows with values of -inf
cod.data.1 <- cod.data.1[cod.data.1$logk!=-Inf,]
#range(cod.data.1$logk)
#data for non-age zero only
cod.data.1plus <- cod.data.1[cod.data.1$ZERO=='N',]

#nullmodels
nullm=lm(logk~1, data=cod.data.1)
nullm.k=lm(logk~1, data=cod.data.1)
nullm.k.dev <- deviance(nullm.k)
nullm.dev <- deviance(nullm)


#MODELS:
#make a table for the results
results.k <- matrix(NA, nrow = 35, ncol = 3)
options(digits = 2)
colnames(results.k)  <- c("model", "AIC","% deviance")
View(results.k)

#plots:
boxplot(logk~SEASON, data = cod.data.1)
boxplot(logk~COHORT + zero, data = cod.data.1,ylab="logk",xlab = "cohort&zero")
boxplot(logk~YEAR+LOCATION, data = cod.data.1,ylab="logk",xlab = "location&year")
abline(h=0)
boxplot(logk~YEAR+zero, data = cod.data.1, ylab = "logk",xlab="year for age 1+(N) and 0(Y)");
abline(h=0)
boxplot(logk~YEAR+SEASON, data = cod.data.1plus, ylab = "logk", xlab = "season + year")
abline(h=0)
boxplot(logk~YEAR+zero, data = cod.dajta.1plus, ylab = "logk",xlab="year for age 1+ ");
abline(h=0)

#k with age as a factor
lm1<-lm(logk~zero, data=cod.data.1)
plot(logk~zero, data=cod.data.1)
summary(lm1)
results.k[1,1] <- "logk~zero"
results.k[1,2] <- round(AIC(lm1),0)
results.k[1,3] <- round((nullm.dev - deviance(lm1))/nullm.dev,2)

#logk~location 
lme7 <- lm(logk~LOCATION, data = cod.data.1)
results.k[2,1] <- "logk~LOCATION"
results.k[2,2] <- round(AIC(lme7),0)
results.k[2,3] <- round((nullm.dev - deviance(lme7))/nullm.dev,2)
#summary(lme7)

#logk~year
lme9 <- lm(logk~YEAR,data = cod.data.1)
results.k[3,1] <- "logk~YEAR"
results.k[3,2] <- round(AIC(lme9),0)
results.k[3,3] <- round((nullm.dev - deviance(lme9))/nullm.dev,2)
#summary(lme9)

#logk~season
lme9.1 <- lm(logk~SEASON,data = cod.data.1)
results.k[4,1] <- "logk~SEASON"
results.k[4,2] <- round(AIC(lme9.1),0)
results.k[4,3] <- round((nullm.dev - deviance(lme9.1))/nullm.dev,2)
#summary(lme9.1)

#logk~cohort
lme9.2 <- lm(logk~COHORT,data = cod.data.1)
results.k[5,1] <- "logk~COHORT"
results.k[5,2] <- round(AIC(lme9.2),0)
results.k[5,3] <- round((nullm.dev - deviance(lme9.2))/nullm.dev,2)
#summary(lme9.2)

#logk~location+year
lme8 <- lm(logk~YEAR+LOCATION, data = cod.data.1)
results.k[6,1] <- "logk~YEAR+LOCATION"
results.k[6,2] <- round(AIC(lme8),0)
results.k[6,3] <- round((nullm.dev - deviance(lme8))/nullm.dev,2)
#summary(lme8)


#logk~YEAR+SEASON
lme10 <- lm(logk~YEAR + SEASON, data = cod.data.1)
results.k[7,1] <- "logk~YEAR+SEASON"
results.k[7,2] <- round(AIC(lme10),0)
results.k[7,3] <- round((nullm.dev - deviance(lme10))/nullm.dev,2)
#summary(lme10)

#logk~SEASON+COHORT
lme11 <- lm(logk~SEASON +COHORT, data = cod.data.1)
results.k[8,1] <- "logk~SEASON+COHORT"
results.k[8,2] <- round(AIC(lme11),0)
results.k[8,3] <- round((nullm.dev - deviance(lme11))/nullm.dev,2)
#summary(lme11)

#logk~LOCATION+COHORT 
lme12 <- lm(logk~LOCATION+ COHORT, data = cod.data.1)
results.k[9,1] <- "logk~LOCATION+COHORT"
results.k[9,2] <- round(AIC(lme12),0)
results.k[9,3] <- round((nullm.dev - deviance(lme12))/nullm.dev,2)
#summary(lme12)

#logk~season+location 
lme13 <- lm(logk~SEASON + LOCATION, data = cod.data.1)
results.k[10,1] <- "logk~SEASON+LOCATION"
results.k[10,2] <- round(AIC(lme13),0)
results.k[10,3] <- round((nullm.dev - deviance(lme13))/nullm.dev,2)
#summary(lme13)

#logk~year+cohort 
lme14 <- lm(logk~YEAR + COHORT, data = cod.data.1)
results.k[11,1] <- "logk~YEAR+COHORT"
results.k[11,2] <- round(AIC(lme14),0)
results.k[11,3] <- round((nullm.dev - deviance(lme14))/nullm.dev,2)
#summary(lme14)

#logk~zero+year+cohort 
lme15 <- lm(logk~ZERO+ YEAR + COHORT, data = cod.data.1)
results.k[12,1] <- "logk~ZERO+YEAR+COHORT"
results.k[12,2] <- round(AIC(lme15),0)
results.k[12,3] <- round((nullm.dev - deviance(lme15))/nullm.dev,2)
#summary(lme15)

#logk~zero+season+cohort
lme16 <- lm(logk~ZERO+SEASON+COHORT, data = cod.data.1)
results.k[13,1] <- "logk~ZERO+SEASON+COHORT"
results.k[13,2] <- round(AIC(lme16),0)
results.k[13,3] <- round((nullm.dev - deviance(lme16))/nullm.dev,2)
#summary(lme16)

#logk~zero+season+location
lme17 <- lm(logk~ZERO+SEASON+LOCATION, data = cod.data.1)
results.k[14,1] <- "logk~ZERO+SEASON+LOCATION"
results.k[14,2] <- round(AIC(lme17),0)
results.k[14,3] <- round((nullm.dev - deviance(lme17))/nullm.dev,2)
#summary(lme17)

#logk~season+location+cohort
lme17.1 <- lm(logk~SEASON+LOCATION+COHORT, data = cod.data.1)
results.k[15,1] <- "logk~SEASON+LOCATION+COHORT"
results.k[15,2] <- round(AIC(lme17.1),0)
results.k[15,3] <- round((nullm.dev - deviance(lme17.1))/nullm.dev,2)
#summary(lme17.1)

#logk~season+location+year
lme17.2 <- lm(logk~SEASON+LOCATION+YEAR, data = cod.data.1)
results.k[16,1] <- "logk~SEASON+LOCATION+YEAR"
results.k[16,2] <- round(AIC(lme17.2),0)
results.k[16,3] <- round((nullm.dev - deviance(lme17.2))/nullm.dev,2)
#summary(lme17.2)

#logk~year+zero+season w all interactions 
lme18 <- lm(logk~ZERO+ YEAR + SEASON, data = cod.data.1)
results.k[17,1] <- "logk~ZERO+YEAR+SEASON"
results.k[17,2] <- round(AIC(lme18),0)
results.k[17,3] <- round((nullm.dev - deviance(lme18))/nullm.dev,2)
#summary(lme18)
#plot(lme18)

#logk~year+zero+location+season 
lme19 <- lm(logk~ZERO + YEAR + LOCATION + SEASON, data = cod.data.1)
results.k[18,1] <- "logk~ZERO+YEAR+LOC+SEAS"
results.k[18,2] <- round(AIC(lme19),0)
results.k[18,3] <- round((nullm.dev - deviance(lme19))/nullm.dev,2)
#summary(lme19)


#logk~zero+ cohort+ location+season w all interactions 
lme20 <- lm(logk~ZERO +COHORT + LOCATION + SEASON,data = cod.data.1)
results.k[19,1] <- "logk~zero+COHORT+LOCATION+SEASON+all two ways"
results.k[19,2] <- AIC(lme20)
results.k[19,3] <- (nullm.dev - deviance(lme20))/nullm.dev
summary(lme20)


#logk~zero+year+location+season
lme21 <- lm(logk~zero + YEAR + LOCATION + SEASON, data = cod.data.1)
results.k[20,1] <- "logk~zero+YEAR+LOCATION+SEASON"
results.k[20,2] <- AIC(lme21)
results.k[20,3] <- (nullm.dev - deviance(lme21))/nullm.dev
summary(lme21)
#plot(lme18)

#logk~year+zero+location+season w all interactions but year*season
lme22 <- lm(logk~zero + YEAR + LOCATION + SEASON + YEAR*LOCATION + LOCATION*SEASON + zero*YEAR + zero*LOCATION + zero*SEASON, data = cod.data.1)
results.k[21,1] <- "logk~zero+YEAR+LOC+SEAS+all two ways except year/seas"
results.k[21,2] <- AIC(lme22)
results.k[21,3] <- (nullm.dev - deviance(lme22))/nullm.dev
summary(lme22)

#logk~year+zero+location+season w only zero*location and zero*year and zero*season
lme23 <- lm(logk~zero + YEAR + LOCATION + SEASON + zero*YEAR + zero*LOCATION + zero*SEASON, data = cod.data.1)
results.k[22,1] <- "logk~zero+YEAR+LOC+SEAS+all zero interactions"
results.k[22,2] <- AIC(lme23)
results.k[22,3] <- (nullm.dev - deviance(lme23))/nullm.dev
summary(lme23)