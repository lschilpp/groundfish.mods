
#Cod Models - age 1
#12/4/15
#data manipulation section:
cod.data <- read.csv("~/Desktop/MS Thesis/cod_age_data.csv")
#add cohort 
cod.data$COHORT = cod.data$YEAR - cod.data$AGE
#to get only the weight data (post 1992)
cod.wt.data <- cod.data[cod.data$YEAR>=1992,]
#add logweight
cod.wt.data$LOGWT = log(cod.wt.data$INDWT)
#add condtion factor
cod.wt.data$K = 100000*(cod.wt.data$INDWT/(cod.wt.data$LENGTH^3))
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
#set year as a factor
cod.wt.data$YEAR=as.factor(cod.wt.data$YEAR);
#install.packages("lme4")
library(lme4)
#install.packages("xtable")
#library(xtable)
#subset data into ages:
cod.data.1 <- cod.wt.data[cod.wt.data$AGE==1,];
#omit lines with missing weights
cod.data.1 <- na.omit(cod.data.1)
#View(cod.data.1$LOGWT)
#add variation from mean weight-at-age
cod.data.1$VAR <- cod.data.1$INDWT-mean(cod.data.1$INDWT)
MNWT = mean(cod.data.1$INDWT)

#MODELS:
#make a table for the results
results <- matrix(NA, nrow = 25, ncol = 8)
colnames(results)  <- c("model", "Intercept"," 1st coefficient", "2nd coefficient","interaction term","3rd","deviance", "AIC")
View(results)

#variation in weight with year as a factor
#lm1<-lm(VAR~YEAR, data=cod.data.1)
#plot(VAR~YEAR, data=cod.data.1)
#abline(lm1)
#summary(lm1)
#results[1,1] <- "VAR~YEAR"
#results[1,2:3] <- coef(lm1)
#results[1,7] <- deviance(lm1)
#results[1,8] <- AIC(lm1)
#View(results)

#logwt with year as the only factor
lm1.2 <- lm(LOGWT~YEAR, data = cod.data.1)
summary(lm1.2)
results[2,1] <- "LOGWT~YEAR"
results[2,2:3] <- coef(lm1.2)
results[2,7] <- deviance(lm1.2)
results[2,8] <- AIC(lm1.2)

which(cod.data.1$LOGWT==-Inf)
range(cod.data.1$LOGWT)

#logweight by season
lme2<-lm(LOGWT~SEASON, data=cod.data.1)
summary(lme2)
results[3,1] <- "LOGWT~SEASON"
results[3,2:3] <- coef(lme2)
results[3,7] <- deviance(lme2)
results[3,8] <- AIC(lme2)
plot(LOGWT~SEASON, data = cod.data.1)

#logweight by LOCATION
lme3<-lm(LOGWT~LOCATION, data = cod.data.1)
summary(lme3)
results[4,1] <- "LOGWT~LOCATION"
results[4,2:3] <- coef(lme3)
results[4,7] <- deviance(lme3)
results[4,8] <- AIC(lme3)

plot(LOGWT~LOCATION, main = "age 1", data = cod.data.1)


#logweight by year and season
lme4<-lm(LOGWT~YEAR+SEASON, data=cod.data.1)
results[5,1] <- "LOGWT~YEAR + SEASON"
results[5,2:4] <- coef(lme4)
results[5,7] <- deviance(lme4)
results[5,8] <- AIC(lme4)
summary(lme4)
#with interaction term
lme4.1 <- lm(LOGWT~YEAR + SEASON + YEAR*SEASON, data = cod.data.1)
results[6,1] <- "LOGWT~YEAR+SEASON+YEAR*SEASON"
results[6,2:5] <- coef(lme4.1)
results[6,7] <- deviance(lme4.1)
results[6,8] <- AIC(lme4.1)
summary(lme4.1)
#logwt by year and location
lme5<-lm(LOGWT~YEAR+LOCATION, data=cod.data.1)
results[7,1] <- "LOGWT~YEAR+LOCATION"
results[7,2:4] <- coef(lme5)
results[7,7] <- deviance(lme5)
results[7,8] <- AIC(lme5)
summary(lme5)
#with interaction term
lme5.1<-lm(LOGWT~YEAR + LOCATION + YEAR*LOCATION, data = cod.data.1)
results[8,1] <- "LOGWT~YEAR+LOCATION+YEAR*LOCATION"
results[8,2:5] <- coef(lme5.1)
results[8,7] <- deviance(lme5.1)
results[8,8] <- AIC(lme5.1)
summary(lme5.1)

#logwt by season and location
lme6<-lm(LOGWT~LOCATION+SEASON, data=cod.data.1)
results[9,1] <- "LOGWT~LOCATION+SEASON"
results[9,2:4] <- coef(lme6)
results[9,7] <- deviance(lme6)
results[9,8] <- AIC(lme6)
summary(lme6)

#with intereaction term
lme6.1<-lm(LOGWT~LOCATION + SEASON + LOCATION*SEASON, data = cod.data.1)
results[10,1] <- "LOGWT~LOCATION+SEASON+LOCATION*SEASON"
results[10,2:5] <- coef(lme6.1)
results[10,7] <- deviance(lme6.1)
results[10,8] <- AIC(lme6.1)
summary(lme6.1)


#with all 3 as factors, with interaction terms
lme6.2<-lm(LOGWT~LOCATION + SEASON + YEAR + LOCATION*SEASON*YEAR, data = cod.data.1)
results[11,1] <- "LOGWT~LOCATION+SEASON+YEAR+LOCATION*SEASON*YEAR"
results[10,2:5] <- coef(lme6.2)
results[11,7] <- deviance(lme6.2)
results[11,8] <- AIC(lme6.2)
summary(lme6.2)

#with all 3 as factors, without interaction terms
lme6.3<-lm(LOGWT~LOCATION + SEASON + YEAR, data = cod.data.1)
results[12,1] <- "LOGWT~LOCATION+SEASON+YEAR"
#results[10,2:5] <- coef(lme6.3)
results[12,7] <- deviance(lme6.3)
results[12,8] <- AIC(lme6.3)
summary(lme6.3)


#K by year
lme7<-lm(K~YEAR, data=cod.data.1)
results[13,1] <- "K~YEAR"
results[13,2:3] <- coef(lme7)
results[13,7] <- deviance(lme7)
results[13,8] <- AIC(lme7)
summary(lme7)
plot(K~YEAR, data=cod.data.1)


#K by season
lm8<-lm(K~1+SEASON, data=cod.data.1)
results[14,1] <- "K~SEASON"
results[14,2:3] <- coef(lm8)
results[14,7] <- deviance(lm8)
results[14,8] <- AIC(lm8)
summary(lm8)
plot(K~SEASON, data = cod.data.1)


#K by LOCATION
lme9<-lm(K~LOCATION, data = cod.data.1)
results[15,1] <- "K~LOCATION"
results[15,2:3] <- coef(lme9)
results[15,7] <- deviance(lme9)
results[15,8] <- AIC(lme9)
summary(lme9)
plot(K~LOCATION, data = cod.data.1)
#residplot
resid.9 <- resid(lme9)
plot(resid.9~YEAR, data= cod.data.1)

#K by year and season
lme10<-lm(K~YEAR+SEASON, data=cod.data.1)
results[16,1] <- "K~YEAR+SEASON"
results[16,2:4] <- coef(lme10)
results[16,7] <- deviance(lme10)
results[16,8] <- AIC(lme10)
summary(lme10)

#adding in interaction term
lme10.1<-lm(K~YEAR + SEASON + YEAR*SEASON, data = cod.data.1)
results[17,1] <- "K~YEAR+SEASON + YEAR*SEASON"
results[17,2:5] <- coef(lme10.1)
results[17,7] <- deviance(lme10.1)
results[17,8] <- AIC(lme10.1)
summary(lme10.1)


#K by year and location
lme11<-lm(K~YEAR+LOCATION, data=cod.data.1)
results[18,1] <- "K~YEAR+LOCATION"
results[18,2:4] <- coef(lme11)
results[18,7] <- deviance(lme11)
results[18,8] <- AIC(lme11)
summary(lme11)

#with interaction term
lme11.1<-lm(K~YEAR+LOCATION+YEAR*LOCATION, data=cod.data.1)
results[19,1] <- "K~YEAR+LOCATION+YEAR*LOCATION"
results[19,2:5] <- coef(lme11.1)
results[19,7] <- deviance(lme11.1)
results[19,8] <- AIC(lme11.1)
summary(lme11.1)


#K by season and location
lme12<-lm(K~LOCATION+SEASON, data=cod.data.1)
results[20,1] <- "K~LOCATION+SEASON"
results[20,2:4] <- coef(lme12)
results[20,7] <- deviance(lme12)
results[20,8] <- AIC(lme12)
summary(lme12)

#adding interaction
lme12.1 <- lm(K~LOCATION+SEASON+LOCATION*SEASON, data = cod.data.1)
results[21,1] <- "K~YLOCATION+SEASON+LOCATION*SEASON"
results[21,2:5] <- coef(lme12.1)
results[21,7] <- deviance(lme12.1)
results[21,8] <- AIC(lme12.1)
summary(lme12.1)


#with all 3 factors + interactions
lme13 <- lm(K~LOCATION + SEASON + YEAR + LOCATION*SEASON + LOCATION*YEAR + SEASON*YEAR, data= cod.data.1)
results[22,1] <- "K with all 3 with all interactions"
results[22,2] <- "see summary"
results[22,7] <- deviance(lme13)
results[22,8] <- AIC(lme13)
summary(lme13)

#with all 3 factors without interactions
lme14 <- lm(K~LOCATION + SEASON + YEAR, data= cod.data.1)
results[23,1] <- "K with all 3 /no interactions"
results[23,2] <- "see summary"
results[23,7] <- deviance(lme14)
results[23,8] <- AIC(lme14)
summary(lme14)

write.table(results,file="results.1.csv",sep=",",row.names=FALSE,quote=FALSE)

