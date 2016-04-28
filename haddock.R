#Haddock script
#Liberty Schilpp
#Haddock Data Models 
#4/8/16
load("/Users/liberty/Desktop/MS Thesis/survey.data.RData")
haddock <- na.omit(haddock.age.data)
length(haddock$INDWT)#has weights, 35366 obs
range(haddock$YEAR)#1992 2015
#This is for the models with all ages combined

#add cohort 
haddock$COHORT = haddock$YEAR - haddock$AGE
#to get only the weight data and ages under 10 (post 1992)
haddock <- haddock[haddock$YEAR>=1992,]
haddock <- haddock[haddock$AGE<=10,]
#add logweight
haddock$LOGWT = log(haddock$INDWT)
#add condtion factor
haddock$K = 100000*(haddock$INDWT/(haddock$LENGTH^3))
#location - Georges Bank = strata 1130 - 1250, GOM = 1260-1400
#Create a new column and fill with NAs
haddock$LOCATION <- NA
#Now assign the locations depending on the values for Stratum
haddock$LOCATION[haddock$STRATUM >= 1130 & haddock$STRATUM <= 1250] <- "GB"
haddock$LOCATION[haddock$STRATUM >= 1260 & haddock$STRATUM <= 1400] <- "GOM"
head(haddock);
plot(LOGWT~STRATUM, data = haddock)
boxplot(LOGWT~LOCATION, data = haddock)
length(haddock$LOCATION)
str(haddock$LOCATION)
#set season as a factor
haddock$SEASON=as.factor(haddock$SEASON);
#set location as a factor
haddock$LOCATION=as.factor(haddock$LOCATION);
#set age as a factor
haddock$AGE=as.factor(haddock$AGE);
#set year as a factor
haddock$YEAR=as.factor(haddock$YEAR);
#set year as a factor
haddock$COHORT=as.factor(haddock$COHORT);
#set maturity as a factor
haddock$MATURITY=as.factor(haddock$MATURITY);
#set SEX as a factor
haddock$SEX=as.factor(haddock$SEX);
#install.packages("lme4")
library(lme4)


#remove rows with values of -inf
haddock <- haddock[haddock$LOGWT!=-Inf,]

#nullmodels
nullm=lm(LOGWT~AGE, data=haddock)
nullm.k=lm(K~1, data=haddock)
nullm.k.dev <- deviance(nullm.k)
nullm.dev <- deviance(nullm)

#add variation from mean weight-at-age
haddock$MNWT = mean(haddock$INDWT)
haddock$VAR <- haddock$INDWT-mean(haddock$INDWT)


#MODELS:
#make a table for the results
haddock.results <- matrix(NA, nrow = 35, ncol = 3)
colnames(haddock.results)  <- c("model", "AIC","% deviance")
View(haddock.results)
# plot(LOGWT~SEASON, data = haddock)
# plot(K~SEASON, data = haddock)
boxplot(LOGWT~LOCATION+YEAR, data = haddock)
abline(h=0)
# plot(K~LOCATION, data = haddock)
# abline(h=1)
# plot(K~AGE, data = haddock);
plot(LOGWT~AGE, data = haddock);

#logweight with age as a factor #this is my new null model
lm1<-lm(LOGWT~AGE, data=haddock)
plot(LOGWT~AGE, data=haddock)
summary(lm1)
haddock.results[1,1] <- "LOGWT~AGE (null model)"
haddock.results[1,2] <- round(AIC(lm1),0)
haddock.results[1,3] <- round((nullm.dev - deviance(lm1))/nullm.dev,2)

# boxplot(LOGWT~YEAR+LOCATION, data = haddock,col = c("green","red"), cex.axis = .6,xlab = "Age/Location", ylab= "Logweight") 
# legend(x="topright",legend=c("GB","GOM"),pch=21,pt.bg =c("green","red"))
# abline(h=0)
boxplot(LOGWT~LOCATION + AGE, data = haddock,xaxt = "n",main="Weight-at-age by location: Georges Bank(GB)and Gulf of Maine(GOM)",outline = FALSE,col = c("orange","blue"),cex.axis = .6,xlab = "Age", ylab= "Ln(weight)") 
legend(x="topright",legend=c("GB","GOM"),pch=21,pt.bg =c("orange","blue"))

axis(side = 1,at = seq(1.5, by=2, length.out=15), labels=0:14)

boxplot(LOGWT~LOCATION+YEAR, data = haddock,xaxt = "n",main="Weight-at-age by year",outline = FALSE,col = c("orange","blue"),cex.axis = .6) 
#legend(x="topright",legend=c("GB","GOM"),pch=21,pt.bg =c("orange","blue"))

axis(side = 1,at = seq(1.5, by=2, length.out=15), labels=0:14)
#logwt~age+location 
lme7 <- lm(LOGWT~AGE+LOCATION, data = haddock)
haddock.results[2,1] <- "LOGWT~AGE+LOCATION"
haddock.results[2,2] <- round(AIC(lme7),0)
haddock.results[2,3] <- round((nullm.dev - deviance(lme7))/nullm.dev,2)
#summary(lme7)

#logwt~age+year
lme9 <- lm(LOGWT~AGE+YEAR,data = haddock)
haddock.results[3,1] <- "LOGWT~AGE+YEAR"
haddock.results[3,2] <- round(AIC(lme9),0)
haddock.results[3,3] <- round((nullm.dev - deviance(lme9))/nullm.dev,2)
#summary(lme9)
coefplot(lme9)

#logwt~age+season
lme9.1 <- lm(LOGWT~AGE+SEASON,data = haddock)
haddock.results[4,1] <- "LOGWT~AGE+SEASON"
haddock.results[4,2] <- round(AIC(lme9.1),0)
haddock.results[4,3] <- round((nullm.dev - deviance(lme9.1))/nullm.dev,2)
#summary(lme9.1)

#logwt~age+cohort
lme9.2 <- lm(LOGWT~AGE+COHORT,data = haddock)
haddock.results[5,1] <- "LOGWT~AGE+COHORT"
haddock.results[5,2] <- round(AIC(lme9.2),0)
haddock.results[5,3] <- round((nullm.dev - deviance(lme9.2))/nullm.dev,2)
#summary(lme9.2)

#logwt~age+sex
lme9.3 <- lm(LOGWT~AGE+SEX,data = haddock)
haddock.results[6,1] <- "LOGWT~AGE+SEX"
haddock.results[6,2] <- round(AIC(lme9.3),0)
haddock.results[6,3] <- round((nullm.dev - deviance(lme9.3))/nullm.dev,2)
#summary(lme9.3)

#logwt~age+maturity
lme9.4 <- lm(LOGWT~AGE+MATURITY,data = haddock)
haddock.results[7,1] <- "LOGWT~AGE+MATURITY"
haddock.results[7,2] <- round(AIC(lme9.4),0)
haddock.results[7,3] <- round((nullm.dev - deviance(lme9.4))/nullm.dev,2)
#summary(lme9.4)

#logwt~age+maturity+location 
lme13 <- lm(LOGWT~AGE+ MATURITY + LOCATION, data = haddock)
haddock.results[8,1] <- "LOGWT~AGE+MATURITY+LOCATION"
haddock.results[8,2] <- round(AIC(lme13),0)
haddock.results[8,3] <- round((nullm.dev - deviance(lme13))/nullm.dev,2)
#summary(lme13)

#logwt~age+maturity+season 
lme14 <- lm(LOGWT~AGE+ MATURITY + SEASON, data = haddock)
haddock.results[9,1] <- "LOGWT~AGE+MATURITY+SEASON"
haddock.results[9,2] <- round(AIC(lme14),0)
haddock.results[9,3] <- round((nullm.dev - deviance(lme14))/nullm.dev,2)
#summary(lme14)

#logwt~age+maturity+cohort 
lme15 <- lm(LOGWT~AGE+ MATURITY + COHORT, data = haddock)
haddock.results[10,1] <- "LOGWT~AGE+MATURITY+COHORT"
haddock.results[10,2] <- round(AIC(lme15),0)
haddock.results[10,3] <- round((nullm.dev - deviance(lme15))/nullm.dev,2)
#summary(lme15)

#logwt~age+maturity+year
lme16 <- lm(LOGWT~AGE+MATURITY+YEAR, data = haddock)
haddock.results[11,1] <- "LOGWT~AGE+MATURITY+YEAR"
haddock.results[11,2] <- round(AIC(lme16),0)
haddock.results[11,3] <- round((nullm.dev - deviance(lme16))/nullm.dev,2)
#summary(lme16)

#logwt~age+maturity+sex
lme17 <- lm(LOGWT~AGE+MATURITY+SEX, data = haddock)
haddock.results[12,1] <- "LOGWT~AGE+MATURITY+SEX"
haddock.results[12,2] <- round(AIC(lme17),0)
haddock.results[12,3] <- round((nullm.dev - deviance(lme17))/nullm.dev,2)
#summary(lme17)

#logwt~age+maturity+season+cohort
lme35 <- lm(LOGWT~AGE+MATURITY+SEASON+SEX, data = haddock)
haddock.results[13,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX"
haddock.results[13,2] <- round(AIC(lme35),0)
haddock.results[13,3] <- round((nullm.dev - deviance(lme35))/nullm.dev,2)
#summary(lme35)

#logwt~age+maturity + season + sex+ location 
lme8 <- lm(LOGWT~AGE+MATURITY+SEASON+SEX+LOCATION, data = haddock)
haddock.results[14,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+LOCATION"
haddock.results[14,2] <- round(AIC(lme8),0)
haddock.results[14,3] <- round((nullm.dev - deviance(lme8))/nullm.dev,2)
#summary(lme8)


#logwt~age+maturity + season + sex+ location + year 
lme8.1 <- lm(LOGWT~AGE+MATURITY+SEASON+SEX+LOCATION+YEAR, data = haddock)
haddock.results[15,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+LOCATION+YEAR"
haddock.results[15,2] <- round(AIC(lme8.1),0)
haddock.results[15,3] <- round((nullm.dev - deviance(lme8.1))/nullm.dev,2)
#summary(lme8.1)

# #logwt~age +maturity+ season+ sex +location+year +cohort
lme11 <- lm(LOGWT~AGE + MATURITY+ SEASON + SEX+ COHORT + YEAR+ LOCATION, data = haddock)
haddock.results[16,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+LOCATION+COHORT+YEAR"
haddock.results[16,2] <- round(AIC(lme11),0)
haddock.results[16,3] <- round((nullm.dev - deviance(lme11))/nullm.dev,2)
coef(summary(lme11))
coefplot(lme11)
# #logwt~age + season +maturity+ sex+ cohort+location+year w age/location
lme12 <- lm(LOGWT~AGE + MATURITY+ SEASON + SEX+ COHORT+ YEAR+ LOCATION+ AGE*LOCATION, data = haddock)
haddock.results[17,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+LOCATION+COHORT+YEAR+AGE*LOCATION"
haddock.results[17,2] <- round(AIC(lme12),0)
haddock.results[17,3] <- round((nullm.dev - deviance(lme12))/nullm.dev,2)
# #summary(lme12)
#ADDED BECAUSE OF PLOT

# #logwt~age + season +maturity+ season+ cohort+location+year w maturity/season
lme13 <- lm(LOGWT~AGE + MATURITY+ SEASON + SEX+ COHORT+ LOCATION + YEAR+ MATURITY*SEASON, data = haddock)
haddock.results[18,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+LOCATION+COHORT+YEAR+MATURITY*SEASON"
haddock.results[18,2] <- round(AIC(lme13),0)
haddock.results[18,3] <- round((nullm.dev - deviance(lme13))/nullm.dev,2)
# #summary(lme13)

# #logwt~age + season +maturity+ season+ cohort+location+year w age/year
lme14 <- lm(LOGWT~AGE + MATURITY+ SEASON + +SEX+ COHORT+ LOCATION + YEAR+ MATURITY*SEX, data = haddock)
haddock.results[19,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+LOC+COHORT+YEAR+MATURITY*SEX"
haddock.results[19,2] <- round(AIC(lme14),0)
haddock.results[19,3] <- round((nullm.dev - deviance(lme14))/nullm.dev,2)
# #summary(lme14)

# #
lme15 <- lm(LOGWT~AGE + MATURITY+ SEASON + SEX+ COHORT+ LOCATION + YEAR+ SEX*SEASON, data = haddock)
haddock.results[20,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+LOC+COHORT+YEAR+SEX*SEASON"
haddock.results[20,2] <- round(AIC(lme15),0)
haddock.results[20,3] <- round((nullm.dev - deviance(lme15))/nullm.dev,2)
# #summary(lme14)

#
lme19 <- lm(LOGWT~AGE + MATURITY + SEASON+SEX+COHORT+LOCATION+YEAR + SEASON*YEAR,data = haddock)
haddock.results[21,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+LOC+COHORT+YEAR+SEAS/YEAR"
haddock.results[21,2] <- round(AIC(lme19),0)
haddock.results[21,3] <- round((nullm.dev - deviance(lme19))/nullm.dev,2)
#summary(lme19)
#plot(lme19)

# 
lme23 <- lm(LOGWT~AGE + MATURITY+ SEASON+SEX+COHORT+ LOCATION + YEAR + MATURITY*AGE, data = haddock)
haddock.results[22,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+COHORT+LOC+YEAR+MATURITY*AGE"
haddock.results[22,2] <- round(AIC(lme23),0)
haddock.results[22,3] <- round((nullm.dev - deviance(lme23))/nullm.dev,2)
#summary(lme23)

#l
lme20 <- lm(LOGWT~AGE +MATURITY + SEX+ LOCATION + SEASON + YEAR + COHORT+SEX*AGE, data = haddock)
haddock.results[23,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+COHORT+LOC+YEAR+SEX*AGE"
haddock.results[23,2] <- round(AIC(lme20),0)
haddock.results[23,3] <- round((nullm.dev - deviance(lme20))/nullm.dev,2)
summary(lme20)
#plot(lme20)


#did this one because of original plots showing some ages w a time trend
lme22 <- lm(LOGWT~AGE + MATURITY + SEX + SEASON + COHORT + LOCATION + YEAR + AGE*YEAR, data = haddock)
haddock.results[24,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+COHORT+LOC+YEAR+YEAR*AGE"
haddock.results[24,2] <- round(AIC(lme22),0)
haddock.results[24,3] <- round((nullm.dev - deviance(lme22))/nullm.dev,2)
coef(summary(lme22))
anova(lme22)

#logwt~age+ year +season w all interactions 
lme28 <- lm(LOGWT~AGE + MATURITY + SEASON + SEX + COHORT+LOCATION + YEAR + LOCATION*YEAR, data = haddock)
haddock.results[25,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+COHORT+LOC+YEAR+YEAR*LOCATION"
haddock.results[25,2] <- round(AIC(lme28),0)
haddock.results[25,3] <- round((nullm.dev - deviance(lme28))/nullm.dev,2)
#summary(lme28)
#anova(lme28)
#plot(lme20)

write.csv(haddock.results,file="haddock.results.csv",row.names=FALSE,quote=FALSE)
# #data exploration for sex and maturity
# head(haddock)
# boxplot(LOGWT~SEX, data = haddock)
# boxplot(LOGWT~MATURITY, data = haddock)
#just for fun
#plot(K~LOGWT, data=haddock)
