#Liberty Schilpp
#Cod Data Models 
#12/8/15#rev 1/18/16, 1/22/16, 1/28/16, revised 2/11/16
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
cod.wt.data$zero <- NA
cod.wt.data$zero[cod.wt.data$AGE >= 1] <- "N"
cod.wt.data$zero[cod.wt.data$AGE < 1] <- "Y"
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
cod.wt.data$zero=as.factor(cod.wt.data$zero);
#install.packages("lme4")
library(lme4)

cod.data.1 <- cod.wt.data;
cod.data.1 <- na.omit(cod.data.1)
#remove rows with values of -inf
cod.data.1 <- cod.data.1[cod.data.1$logk!=-Inf,]
#range(cod.data.1$logk)

#nullmodels
nullm=lm(logk~1, data=cod.data.1)
nullm.k=lm(logk~1, data=cod.data.1)
nullm.k.dev <- deviance(nullm.k)
nullm.dev <- deviance(nullm)

#add variation from mean weight-at-age
MNWT = mean(cod.data.1$INDWT)
cod.data.1$VAR <- cod.data.1$INDWT-mean(cod.data.1$INDWT)


#MODELS:
#make a table for the results
results.k <- matrix(NA, nrow = 35, ncol = 3)
options(digits = 2)
colnames(results.k)  <- c("model", "AIC","% deviance")
View(results.k)

#plots:
plot(logk~SEASON, data = cod.data.1)
boxplot(logk~COHORT + zero, data = cod.data.1,ylab="logk",xlab = "cohort&zero")
boxplot(logk~LOCATION +zero, data = cod.data.1,ylab="logk",xlab = "location&zeroage")
boxplot(logk~YEAR+zero, data = cod.data.1, ylab = "logk",xlab="year for age 1+(N) and 0(Y)");


#k with age as a factor
lm1<-lm(logk~AGE, data=cod.data.1)
plot(logk~AGE, data=cod.data.1)
summary(lm1)
results.k[1,1] <- "logk~AGE"
results.k[1,2] <- AIC(lm1)
results.k[1,3] <- (nullm.dev - deviance(lm1))/nullm.dev

#logk~age+location 
lme7 <- lm(logk~AGE+LOCATION, data = cod.data.1)
results.k[2,1] <- "logk~AGE+LOCATION"
results.k[2,2] <- AIC(lme7)
results.k[2,3] <- (nullm.dev - deviance(lme7))/nullm.dev
summary(lme7)

#logk~age+year
lme9 <- lm(logk~AGE+YEAR,data = cod.data.1)
results.k[3,1] <- "logk~AGE+YEAR"
results.k[3,2] <- AIC(lme9)
results.k[3,3] <- (nullm.dev - deviance(lme9))/nullm.dev
summary(lme9)

#logk~age+season
lme9.1 <- lm(logk~AGE+SEASON,data = cod.data.1)
results.k[4,1] <- "logk~AGE+SEASON"
results.k[4,2] <- AIC(lme9.1)
results.k[4,3] <- (nullm.dev - deviance(lme9.1))/nullm.dev
summary(lme9.1)

#logk~age+cohort
lme9.2 <- lm(logk~AGE+COHORT,data = cod.data.1)
results.k[5,1] <- "logk~AGE+COHORT"
results.k[5,2] <- AIC(lme9.2)
results.k[5,3] <- (nullm.dev - deviance(lme9.2))/nullm.dev
summary(lme9.2)

#logk~age+location w interaction
lme8 <- lm(logk~AGE+LOCATION + LOCATION*AGE, data = cod.data.1)
results.k[6,1] <- "logk~AGE+LOCATION+LOCATION*AGE"
results.k[6,2] <- AIC(lme8)
results.k[6,3] <- (nullm.dev - deviance(lme8))/nullm.dev
summary(lme8)


#logk~AGE+ YEAR w interaction
lme10 <- lm(logk~AGE + YEAR + YEAR*AGE, data = cod.data.1)
results.k[7,1] <- "logk~AGE+YEAR+YEAR*AGE"
results.k[7,2] <- AIC(lme10)
results.k[7,3] <- (nullm.dev - deviance(lme10))/nullm.dev
summary(lme10)

#logk~AGE+ SEASON w interaction
lme11 <- lm(logk~AGE + SEASON + AGE*SEASON, data = cod.data.1)
results.k[8,1] <- "logk~AGE+SEASON+SEASON*AGE"
results.k[8,2] <- AIC(lme11)
results.k[8,3] <- (nullm.dev - deviance(lme11))/nullm.dev
summary(lme11)

#logk~AGE+ COHORT w interaction
lme12 <- lm(logk~AGE + COHORT + AGE*COHORT, data = cod.data.1)
results.k[9,1] <- "logk~AGE+COHORT+AGE*COHORT"
results.k[9,2] <- AIC(lme12)
results.k[9,3] <- (nullm.dev - deviance(lme12))/nullm.dev
summary(lme12)

#logk~age+year+location 
lme13 <- lm(logk~AGE+ YEAR + LOCATION, data = cod.data.1)
results.k[10,1] <- "logk~AGE+YEAR+LOCATION"
results.k[10,2] <- AIC(lme13)
results.k[10,3] <- (nullm.dev - deviance(lme13))/nullm.dev
summary(lme13)

#logk~age+year+season 
lme14 <- lm(logk~AGE+ YEAR + SEASON, data = cod.data.1)
results.k[11,1] <- "logk~AGE+YEAR+SEASON"
results.k[11,2] <- AIC(lme14)
results.k[11,3] <- (nullm.dev - deviance(lme14))/nullm.dev
summary(lme14)

#logk~age+year+cohort 
lme15 <- lm(logk~AGE+ YEAR + COHORT, data = cod.data.1)
results.k[12,1] <- "logk~AGE+YEAR+COHORT"
results.k[12,2] <- AIC(lme15)
results.k[12,3] <- (nullm.dev - deviance(lme15))/nullm.dev
summary(lme15)

#logk~age+season+cohort
lme16 <- lm(logk~AGE+SEASON+COHORT, data = cod.data.1)
results.k[13,1] <- "logk~AGE+SEASON+COHORT"
results.k[13,2] <- AIC(lme16)
results.k[13,3] <- (nullm.dev - deviance(lme16))/nullm.dev
summary(lme16)

#logk~age+season+location
lme17 <- lm(logk~AGE+SEASON+LOCATION, data = cod.data.1)
results.k[14,1] <- "logk~AGE+SEASON+LOCATION"
results.k[14,2] <- AIC(lme17)
results.k[14,3] <- (nullm.dev - deviance(lme17))/nullm.dev
summary(lme17)

#logk~age+season+location+age*location
lme17.1 <- lm(logk~AGE+SEASON+LOCATION+AGE*LOCATION, data = cod.data.1)
results.k[15,1] <- "logk~AGE+SEASON+LOCATION+age*location"
results.k[15,2] <- AIC(lme17.1)
results.k[15,3] <- (nullm.dev - deviance(lme17.1))/nullm.dev
summary(lme17.1)

#logk~age+season+location+age*location+age*season
lme17.2 <- lm(logk~AGE+SEASON+LOCATION+AGE*LOCATION+AGE*SEASON, data = cod.data.1)
results.k[16,1] <- "logk~AGE+SEASON+LOCATION+age*location+AGE*season"
results.k[16,2] <- AIC(lme17.2)
results.k[16,3] <- (nullm.dev - deviance(lme17.2))/nullm.dev
summary(lme17.2)

#logk~year+age+location+season w all interactions 
lme18 <- lm(logk~AGE + YEAR + LOCATION + SEASON + YEAR*LOCATION + LOCATION*SEASON + YEAR*SEASON + AGE*YEAR + AGE*LOCATION + AGE * SEASON, data = cod.data.1)
results.k[17,1] <- "logk~AGE+YEAR+LOCATION+SEASON+all two ways"
results.k[17,2] <- AIC(lme18)
results.k[17,3] <- (nullm.dev - deviance(lme18))/nullm.dev
summary(lme18)
plot(lme18)

#logk~year+age+location+season w all interactions but location*season
lme19 <- lm(logk~AGE + YEAR + LOCATION + SEASON + YEAR*LOCATION + YEAR*SEASON + AGE*YEAR + AGE*LOCATION + AGE * SEASON, data = cod.data.1)
results.k[18,1] <- "logk~AGE+YEAR+LOC+SEAS+all two ways except loc/seas"
results.k[18,2] <- AIC(lme19)
results.k[18,3] <- (nullm.dev - deviance(lme19))/nullm.dev
summary(lme19)


#logk~age+ cohort+ location+season w all interactions 
lme20 <- lm(logk~AGE +COHORT + LOCATION + SEASON + COHORT*LOCATION + LOCATION*SEASON + COHORT*SEASON + AGE*YEAR + AGE*LOCATION + AGE * SEASON, data = cod.data.1)
results.k[19,1] <- "logk~AGE+COHORT+LOCATION+SEASON+all two ways"
results.k[19,2] <- AIC(lme20)
results.k[19,3] <- (nullm.dev - deviance(lme20))/nullm.dev
summary(lme20)


#logk~age+year+location+season
lme21 <- lm(logk~AGE + YEAR + LOCATION + SEASON, data = cod.data.1)
results.k[20,1] <- "logk~AGE+YEAR+LOCATION+SEASON"
results.k[20,2] <- AIC(lme21)
results.k[20,3] <- (nullm.dev - deviance(lme21))/nullm.dev
summary(lme21)
#plot(lme18)

#logk~year+age+location+season w all interactions but year*season
lme22 <- lm(logk~AGE + YEAR + LOCATION + SEASON + YEAR*LOCATION + LOCATION*SEASON + AGE*YEAR + AGE*LOCATION + AGE*SEASON, data = cod.data.1)
results.k[21,1] <- "logk~AGE+YEAR+LOC+SEAS+all two ways except year/seas"
results.k[21,2] <- AIC(lme22)
results.k[21,3] <- (nullm.dev - deviance(lme22))/nullm.dev
summary(lme22)

#logk~year+age+location+season w only age*location and age*year and age*season
lme23 <- lm(logk~AGE + YEAR + LOCATION + SEASON + AGE*YEAR + AGE*LOCATION + AGE*SEASON, data = cod.data.1)
results.k[22,1] <- "logk~AGE+YEAR+LOC+SEAS+all age interactions"
results.k[22,2] <- AIC(lme23)
results.k[22,3] <- (nullm.dev - deviance(lme23))/nullm.dev
summary(lme23)