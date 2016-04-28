#Liberty Schilpp
#Cod Data Models for Adv. Pop. Models Final Project
#12/8/15
#This is for the models with all ages combined
#AND SETTING YEAR AND COHORT AS A FACTOR
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
#set YEAR and COHORT as factors
cod.wt.data$COHORT=as.factor(cod.wt.data$COHORT);
cod.wt.data$YEAR=as.factor(cod.wt.data$YEAR);
#install.packages("lme4")
library(lme4)

cod.data.1 <- cod.wt.data;
cod.data.1 <- na.omit(cod.data.1)
#remove rows with values of -inf
cod.data.1 <- cod.data.1[cod.data.1$LOGWT!=-Inf,]
range(cod.data.1$LOGWT)

#add variation from mean weight-at-age
cod.data.1$VAR <- cod.data.1$INDWT-mean(cod.data.1$INDWT)
MNWT = mean(cod.data.1$INDWT)

#couple plots
head(cod.data.1)
cod.data.spring <- cod.data.1[cod.data.1$SEASON == "SPRING",]
plot(LOGWT~LOCATION, data=cod.data.spring);
cod.data.fall <- cod.data.1[cod.data.1$SEASON == "FALL",]
plot(LOGWT~LOCATION, data=cod.data.fall);
cod.data.spring <- cod.data.1[cod.data.1$SEASON == "SPRING",]
plot(K~LOCATION, main = "spring", data=cod.data.spring);
cod.data.fall <- cod.data.1[cod.data.1$SEASON == "FALL",]
plot(K~LOCATION, main = "fall", data=cod.data.fall);
par(mfrow=c(2,2))
plot(K~LOCATION, main = "K~LOCATION", data = cod.data.1)
plot(LOGWT~LOCATION, main = "LOGWT~LOCATION", data = cod.data.1)
plot(LOGWT~SEASON, main = "LOGWT~SEASON", data = cod.data.1)
plot(K~SEASON, main = "K~SEASON", data = cod.data.1)
#MODELS:
#make a table for the results
results <- matrix(NA, nrow = 34, ncol = 8)
colnames(results)  <- c("model", "Intercept"," 1st coefficient", "2nd coefficient","interaction term","3rd","deviance", "AIC")
View(results)

#with all 3 factors + interactions
lme1 <- lm(LOGWT~LOCATION + SEASON + YEAR + LOCATION*SEASON + LOCATION*YEAR + SEASON*YEAR, data= cod.data.1)
results[1,1] <- "K~YEAR+SEASON+LOCATION+L*S+L*Y+Y*S"
r#esults[20,2] <- "see summary"
results[1,7] <- deviance(lme1)
results[1,8] <- AIC(lme1)
summary(lme1)
resid.1 <- resid(lme1)
par(mfrow=c(1,2))
hist(resid.1, main = "resid.LOGWT-lme1")
plot(resid.13~YEAR,data = cod.data.1)
AIC(lme1)
deviance(lme1)
#logwt with year as the only factor
lm1.2 <- lm(LOGWT~YEAR, data = cod.data.1)
summary(lm1.2)
results[2,1] <- "LOGWT~YEAR"
#results[2,2:3] <- coef(lm1.2)
results[2,7] <- deviance(lm1.2)
results[2,8] <- AIC(lm1.2)

#logweight by season
lme2<-lm(LOGWT~SEASON, data=cod.data.1)
summary(lme2)
results[3,1] <- "LOGWT~SEASON"
#results[3,2:3] <- coef(lme2)
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

plot(LOGWT~LOCATION, data = cod.data.1)


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

#K by year
lme7<-lm(K~YEAR, data=cod.data.1)
results[11,1] <- "K~YEAR"
results[11,2:3] <- coef(lme7)
results[11,7] <- deviance(lme7)
results[11,8] <- AIC(lme7)
summary(lme7)
plot(K~YEAR, data=cod.data.1)


#K by season
lm8<-lm(K~1+SEASON, data=cod.data.1)
results[12,1] <- "K~SEASON"
results[12,2:3] <- coef(lm8)
results[12,7] <- deviance(lm8)
results[12,8] <- AIC(lm8)
summary(lm8)
plot(K~SEASON, data = cod.data.1)


#K by LOCATION
lme9<-lm(K~LOCATION, data = cod.data.1)
results[13,1] <- "K~LOCATION"
results[13,2:3] <- coef(lme9)
results[13,7] <- deviance(lme9)
results[13,8] <- AIC(lme9)
summary(lme9)
plot(K~LOCATION, data = cod.data.1)
#not sure to use these resid plots
resid.9 <- resid(lme9)
plot(resid.9~YEAR, data= cod.data.1)

#K by year and season
lme10<-lm(K~YEAR+SEASON, data=cod.data.1)
results[14,1] <- "K~YEAR+SEASON"
results[14,2:4] <- coef(lme10)
results[14,7] <- deviance(lme10)
results[14,8] <- AIC(lme10)
summary(lme10)

#adding in interaction term
lme10.1<-lm(K~YEAR + SEASON + YEAR*SEASON, data = cod.data.1)
results[15,1] <- "K~YEAR+SEASON + YEAR*SEASON"
results[15,2:5] <- coef(lme10.1)
results[15,7] <- deviance(lme10.1)
results[15,8] <- AIC(lme10.1)
summary(lme10.1)


#K by year and location
lme11<-lm(K~YEAR+LOCATION, data=cod.data.1)
results[16,1] <- "K~YEAR+LOCATION"
results[16,2:4] <- coef(lme11)
results[16,7] <- deviance(lme11)
results[16,8] <- AIC(lme11)
summary(lme11)

#with interaction term
lme11.1<-lm(K~YEAR+LOCATION+YEAR*LOCATION, data=cod.data.1)
results[17,1] <- "K~YEAR+LOCATION+YEAR*LOCATION"
results[17,2:5] <- coef(lme11.1)
results[17,7] <- deviance(lme11.1)
results[17,8] <- AIC(lme11.1)
summary(lme11.1)


#K by season and location
lme12<-lm(K~LOCATION+SEASON, data=cod.data.1)
results[18,1] <- "K~LOCATION+SEASON"
results[18,2:4] <- coef(lme12)
results[18,7] <- deviance(lme12)
results[18,8] <- AIC(lme12)
summary(lme12)

#adding interaction
lme12.1 <- lm(K~LOCATION+SEASON+LOCATION*SEASON, data = cod.data.1)
results[19,1] <- "K~YLOCATION+SEASON+LOCATION*SEASON"
results[19,2:5] <- coef(lme12.1)
results[19,7] <- deviance(lme12.1)
results[19,8] <- AIC(lme12.1)
summary(lme12.1)


#with all 3 factors + interactions
lme13 <- lm(K~LOCATION + SEASON + YEAR + LOCATION*SEASON + LOCATION*YEAR + SEASON*YEAR, data= cod.data.1)
results[20,1] <- "K~YEAR+SEASON+LOCATION+L*S+L*Y+Y*S"
results[20,2] <- "see summary"
results[20,7] <- deviance(lme13)
results[20,8] <- AIC(lme13)
summary(lme13)
resid.13 <- resid(lme13)
hist(resid.13, main = "residuals for K~ location+season+year+L*S + L*Y + S*Y")
plot(resid.13~YEAR, main = "residuals for K~ location+season+year+L*S + L*Y + S*Y", data = cod.data.1)
#nullm=lm(LOGWT~1, data=cod.data.1)
####MODELS INCLUDING COHORT########
#with cohort as the only factor
lm1.3 <- lm(LOGWT~COHORT, data = cod.data.1)
summary(lm1.3)
results[21,1] <- "LOGWT~COHORT"
#results[21,2:3] <- coef(lm1.3)
results[21,7] <- deviance(lm1.3)
results[21,8] <- AIC(lm1.3)

#with cohort and location
lm1.4 <- lm(LOGWT~COHORT+LOCATION, data = cod.data.1)
summary(lm1.4)
results[22,1] <- "LOGWT~COHORT+LOCATION"
results[22,7] <- deviance(lm1.4)
results[22,8] <- AIC(lm1.4)

#with cohort and location and interaction
lm1.5 <- lm(LOGWT~COHORT+LOCATION+COHORT*LOCATION, data = cod.data.1)
summary(lm1.5)
results[23,1] <- "LOGWT~COHORT+LOCATION+COHORT*LOCATION"
results[23,7] <- deviance(lm1.5)
results[23,8] <- AIC(lm1.5)

#with cohort and season
lm1.6 <- lm(LOGWT~COHORT+SEASON, data = cod.data.1)
summary(lm1.6)
results[24,1] <- "LOGWT~COHORT+SEASON"
results[24,7] <- deviance(lm1.6)
results[24,8] <- AIC(lm1.6)

#with cohort and season and interaction
lm1.7 <- lm(LOGWT~COHORT+SEASON+COHORT*SEASON, data = cod.data.1)
summary(lm1.7)
results[25,1] <- "LOGWT~COHORT+SEASON+COHORT*SEASON"
results[25,7] <- deviance(lm1.7)
results[25,8] <- AIC(lm1.7)
resid.1.7 <- resid(lm1.7)
plot(resid.1.7~YEAR, data = cod.data.1, main = "residuals LOGWT~COHORT+SEASON+COHORT*SEASON")

#with cohort and season and location 
lm55 <- lm(LOGWT~COHORT+SEASON+LOCATION, data = cod.data.1)
summary(lm55)
results[26,1] <- "LOGWT~COHORT+SEASON+LOCATION"
results[26,7] <- deviance(lm55)
results[26,8] <- AIC(lm55)

#with cohort and season and location and interaction
lm56 <- lm(LOGWT~COHORT+SEASON+LOCATION+COHORT*SEASON + LOCATION*SEASON + COHORT*LOCATION, data = cod.data.1)
summary(lm56)
results[27,1] <- "LOGWT~COHORT+SEASON+LOCATION+COHORT*LOCATION*SEASON"
results[27,7] <- deviance(lm56)
results[27,8] <- AIC(lm56)
resid.56 <- resid(lm56)
AIC(lm56)
hist(resid.56, data=cod.data.1, main = "residuals-LOGWT~C+S+L+interactions")
plot(resid.56~COHORT, data = cod.data.1, main = "Residuals - LOGWT~C+S+L+interactions")
#with YEAR and season and location and interaction
lm57 <- lm(LOGWT~YEAR+SEASON+LOCATION+YEAR*SEASON+LOCATION*SEASON+YEAR*LOCATION, data = cod.data.1)
summary(lm57)
results[34,1] <- "LOGWT~YEAR+SEASON+LOCATION+YEAR*LOCATION + YEAR*SEASON + LOCAITON*SEASON"
results[34,7] <- deviance(lm57)
results[34,8] <- AIC(lm57)

####MODELS INCLUDING COHORT########
#with cohort as the only factor
lm20 <- lm(K~COHORT, data = cod.data.1)
summary(lm20)
results[28,1] <- "K~COHORT"
#results[21,2:3] <- coef(lm1.3)
results[28,7] <- deviance(lm20)
results[28,8] <- AIC(lm20)

#with cohort and location
lm21 <- lm(K~COHORT+LOCATION, data = cod.data.1)
summary(lm21)
results[29,1] <- "K~COHORT+LOCATION"
results[29,7] <- deviance(lm21)
results[29,8] <- AIC(lm21)

#with cohort and location and interaction
lm22 <- lm(K~COHORT+LOCATION+COHORT*LOCATION, data = cod.data.1)
summary(lm22)
results[30,1] <- "K~COHORT+LOCATION+cohort*location"
results[30,7] <- deviance(lm22)
results[30,8] <- AIC(lm22)

#with cohort and season
lm23 <- lm(K~COHORT+SEASON, data = cod.data.1)
summary(lm23)
results[31,1] <- "K~COHORT+SEASON"
results[31,7] <- deviance(lm23)
results[31,8] <- AIC(lm23)

#with cohort and season and interaction
lm24 <- lm(K~COHORT+SEASON+COHORT*SEASON, data = cod.data.1)
summary(lm24)
results[32,1] <- "K~COHORT+SEASON +COHORT*SEASON"
results[32,7] <- deviance(lm24)
results[32,8] <- AIC(lm24)

#with all 3 facotrs, cohort instead of year
lme25 <- lm(K~LOCATION + SEASON + COHORT + LOCATION*SEASON + LOCATION*COHORT + SEASON*COHORT, data= cod.data.1)
results[33,1] <- "K~ COHORT+ SEASON+ LOCATION +L*S + L*C + S*C"
#results[20,2] <- "see summary"
results[33,7] <- deviance(lme25)
results[33,8] <- AIC(lme25)
summary(lme25)
resid.25 <- resid(lme25)
hist(resid.25, main="residuals K~C+S+L+interactions", xlab = "residuals")
plot(resid.25~YEAR, data = cod.data.1, xlab = "year", main = "residuals K~C+S+L+interactions")


#other plots
plot(LOGWT~LOCATION+SEASON, data=cod.data.1)
plot(LOGWT~SEASON, data=cod.data.1)
plot(K~LOCATION+SEASON, data=cod.data.1)
write.table(results,file="results.agg.csv",sep=",",row.names=FALSE,quote=FALSE)