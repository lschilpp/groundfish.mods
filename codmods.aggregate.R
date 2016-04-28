#Liberty Schilpp
#Cod Data Models 
#12/8/15#rev 1/18/16, 1/22/16, 1/28/16,2/19/16,3/3/16, 3/17/16,3/23/16
#This is for the models with all ages combined
#data manipulation section:
cod.data <- read.csv("~/Desktop/MS Thesis/cod_age_data.csv")
#add cohort 
cod.data$COHORT = cod.data$YEAR - cod.data$AGE
#to get only the weight data and ages under 10 (post 1992)
cod.wt.data <- cod.data[cod.data$YEAR>=1992,]
cod.wt.data <- cod.wt.data[cod.wt.data$AGE<=10,]
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
#set age as a factor
cod.wt.data$AGE=as.factor(cod.wt.data$AGE);
#set year as a factor
cod.wt.data$YEAR=as.factor(cod.wt.data$YEAR);
#set year as a factor
cod.wt.data$COHORT=as.factor(cod.wt.data$COHORT);
#set maturity as a factor
cod.wt.data$MATURITY=as.factor(cod.wt.data$MATURITY);
#set SEX as a factor
cod.wt.data$SEX=as.factor(cod.wt.data$SEX);
#install.packages("lme4")
library(lme4)

cod.data.1 <- cod.wt.data;
cod.data.1 <- na.omit(cod.data.1)
#remove rows with values of -inf
cod.data.1 <- cod.data.1[cod.data.1$LOGWT!=-Inf,]

#nullmodels
nullm=lm(LOGWT~AGE, data=cod.data.1)
nullm.k=lm(K~1, data=cod.data.1)
nullm.k.dev <- deviance(nullm.k)
nullm.dev <- deviance(nullm)

#add variation from mean weight-at-age
cod.data.1$MNWT = mean(cod.data.1$INDWT)
cod.data.1$VAR <- cod.data.1$INDWT-mean(cod.data.1$INDWT)


#MODELS:
#make a table for the results
results.agg <- matrix(NA, nrow = 35, ncol = 3)
colnames(results.agg)  <- c("model", "AIC","% deviance")
View(results.agg)
# plot(LOGWT~SEASON, data = cod.data.1)
# plot(K~SEASON, data = cod.data.1)
boxplot(LOGWT~LOCATION+YEAR, data = cod.data.1)
abline(h=0)
# plot(K~LOCATION, data = cod.data.1)
# abline(h=1)
# plot(K~AGE, data = cod.data.1);
# plot(LOGWT~AGE, data = cod.data.1);

#logweight with age as a factor #this is my new null model
lm1<-lm(LOGWT~AGE, data=cod.data.1)
plot(LOGWT~AGE, data=cod.data.1)
summary(lm1)
results.agg[1,1] <- "LOGWT~AGE (null model)"
results.agg[1,2] <- round(AIC(lm1),0)
results.agg[1,3] <- round((nullm.dev - deviance(lm1))/nullm.dev,2)

# boxplot(LOGWT~YEAR+LOCATION, data = cod.data.1,col = c("green","red"), cex.axis = .6,xlab = "Age/Location", ylab= "Logweight") 
# legend(x="topright",legend=c("GB","GOM"),pch=21,pt.bg =c("green","red"))
# abline(h=0)
boxplot(LOGWT~LOCATION + AGE, data = cod.data.1,xaxt = "n",outline = FALSE,col = c("black","white"),cex.axis = .6,xlab = "Age", ylab= "Ln(weight)") 
#legend(x="topright",legend=c("GB","GOM"),pch=21,pt.bg =c("black","white"))

axis(side = 1,at = seq(1.5, by=2, length.out=15), labels=0:14)
#logwt~age+location 
lme7 <- lm(LOGWT~AGE+LOCATION, data = cod.data.1)
results.agg[2,1] <- "LOGWT~AGE+LOCATION"
results.agg[2,2] <- round(AIC(lme7),0)
results.agg[2,3] <- round((nullm.dev - deviance(lme7))/nullm.dev,2)
#summary(lme7)

#logwt~age+year
lme9 <- lm(LOGWT~AGE+YEAR,data = cod.data.1)
results.agg[3,1] <- "LOGWT~AGE+YEAR"
results.agg[3,2] <- round(AIC(lme9),0)
results.agg[3,3] <- round((nullm.dev - deviance(lme9))/nullm.dev,2)
#summary(lme9)
coefplot(lme9)

#logwt~age+season
lme9.1 <- lm(LOGWT~AGE+SEASON,data = cod.data.1)
results.agg[4,1] <- "LOGWT~AGE+SEASON"
results.agg[4,2] <- round(AIC(lme9.1),0)
results.agg[4,3] <- round((nullm.dev - deviance(lme9.1))/nullm.dev,2)
#summary(lme9.1)

#logwt~age+cohort
lme9.2 <- lm(LOGWT~AGE+COHORT,data = cod.data.1)
results.agg[5,1] <- "LOGWT~AGE+COHORT"
results.agg[5,2] <- round(AIC(lme9.2),0)
results.agg[5,3] <- round((nullm.dev - deviance(lme9.2))/nullm.dev,2)
#summary(lme9.2)

#logwt~age+sex
lme9.3 <- lm(LOGWT~AGE+SEX,data = cod.data.1)
results.agg[6,1] <- "LOGWT~AGE+SEX"
results.agg[6,2] <- round(AIC(lme9.3),0)
results.agg[6,3] <- round((nullm.dev - deviance(lme9.3))/nullm.dev,2)
#summary(lme9.3)

#logwt~age+maturity
lme9.4 <- lm(LOGWT~AGE+MATURITY,data = cod.data.1)
results.agg[7,1] <- "LOGWT~AGE+MATURITY"
results.agg[7,2] <- round(AIC(lme9.4),0)
results.agg[7,3] <- round((nullm.dev - deviance(lme9.4))/nullm.dev,2)
#summary(lme9.4)

#logwt~age+maturity+location 
lme13 <- lm(LOGWT~AGE+ MATURITY + LOCATION, data = cod.data.1)
results.agg[8,1] <- "LOGWT~AGE+MATURITY+LOCATION"
results.agg[8,2] <- round(AIC(lme13),0)
results.agg[8,3] <- round((nullm.dev - deviance(lme13))/nullm.dev,2)
#summary(lme13)

#logwt~age+maturity+season 
lme14 <- lm(LOGWT~AGE+ MATURITY + SEASON, data = cod.data.1)
results.agg[9,1] <- "LOGWT~AGE+MATURITY+SEASON"
results.agg[9,2] <- round(AIC(lme14),0)
results.agg[9,3] <- round((nullm.dev - deviance(lme14))/nullm.dev,2)
#summary(lme14)

#logwt~age+maturity+cohort 
lme15 <- lm(LOGWT~AGE+ MATURITY + COHORT, data = cod.data.1)
results.agg[10,1] <- "LOGWT~AGE+MATURITY+COHORT"
results.agg[10,2] <- round(AIC(lme15),0)
results.agg[10,3] <- round((nullm.dev - deviance(lme15))/nullm.dev,2)
#summary(lme15)

#logwt~age+maturity+year
lme16 <- lm(LOGWT~AGE+MATURITY+YEAR, data = cod.data.1)
results.agg[11,1] <- "LOGWT~AGE+MATURITY+YEAR"
results.agg[11,2] <- round(AIC(lme16),0)
results.agg[11,3] <- round((nullm.dev - deviance(lme16))/nullm.dev,2)
#summary(lme16)

#logwt~age+maturity+sex
lme17 <- lm(LOGWT~AGE+MATURITY+SEX, data = cod.data.1)
results.agg[12,1] <- "LOGWT~AGE+MATURITY+SEX"
results.agg[12,2] <- round(AIC(lme17),0)
results.agg[12,3] <- round((nullm.dev - deviance(lme17))/nullm.dev,2)
#summary(lme17)

#logwt~age+maturity+season+cohort
lme35 <- lm(LOGWT~AGE+MATURITY+SEASON+SEX, data = cod.data.1)
results.agg[13,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX"
results.agg[13,2] <- round(AIC(lme35),0)
results.agg[13,3] <- round((nullm.dev - deviance(lme35))/nullm.dev,2)
#summary(lme35)

#logwt~age+maturity + season + sex+ location 
lme8 <- lm(LOGWT~AGE+MATURITY+SEASON+SEX+LOCATION, data = cod.data.1)
results.agg[14,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+LOCATION"
results.agg[14,2] <- round(AIC(lme8),0)
results.agg[14,3] <- round((nullm.dev - deviance(lme8))/nullm.dev,2)
#summary(lme8)


#logwt~age+maturity + season + sex+ location + year 
lme8.1 <- lm(LOGWT~AGE+MATURITY+SEASON+SEX+LOCATION+YEAR, data = cod.data.1)
results.agg[15,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+LOCATION+YEAR"
results.agg[15,2] <- round(AIC(lme8.1),0)
results.agg[15,3] <- round((nullm.dev - deviance(lme8.1))/nullm.dev,2)
#summary(lme8.1)

# #logwt~age +maturity+ season+ sex +location+year +cohort
lme11 <- lm(LOGWT~AGE + MATURITY+ SEASON + SEX+ COHORT + YEAR+ LOCATION, data = cod.data.1)
results.agg[16,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+LOCATION+COHORT+YEAR"
results.agg[16,2] <- round(AIC(lme11),0)
results.agg[16,3] <- round((nullm.dev - deviance(lme11))/nullm.dev,2)
coef(summary(lme11))
coefplot(lme11)
# #logwt~age + season +maturity+ sex+ cohort+location+year w age/location
lme12 <- lm(LOGWT~AGE + MATURITY+ SEASON + SEX+ COHORT+ YEAR+ LOCATION+ AGE*LOCATION, data = cod.data.1)
results.agg[17,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+LOCATION+COHORT+YEAR+AGE*LOCATION"
results.agg[17,2] <- round(AIC(lme12),0)
results.agg[17,3] <- round((nullm.dev - deviance(lme12))/nullm.dev,2)
# #summary(lme12)
#ADDED BECAUSE OF PLOT

# #logwt~age + season +maturity+ season+ cohort+location+year w maturity/season
lme13 <- lm(LOGWT~AGE + MATURITY+ SEASON + SEX+ COHORT+ LOCATION + YEAR+ MATURITY*SEASON, data = cod.data.1)
results.agg[18,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+LOCATION+COHORT+YEAR+MATURITY*SEASON"
results.agg[18,2] <- round(AIC(lme13),0)
results.agg[18,3] <- round((nullm.dev - deviance(lme13))/nullm.dev,2)
# #summary(lme13)

# #logwt~age + season +maturity+ season+ cohort+location+year w age/year
lme14 <- lm(LOGWT~AGE + MATURITY+ SEASON + +SEX+ COHORT+ LOCATION + YEAR+ MATURITY*SEX, data = cod.data.1)
results.agg[19,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+LOC+COHORT+YEAR+MATURITY*SEX"
results.agg[19,2] <- round(AIC(lme14),0)
results.agg[19,3] <- round((nullm.dev - deviance(lme14))/nullm.dev,2)
# #summary(lme14)

# #
lme15 <- lm(LOGWT~AGE + MATURITY+ SEASON + SEX+ COHORT+ LOCATION + YEAR+ SEX*SEASON, data = cod.data.1)
results.agg[20,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+LOC+COHORT+YEAR+SEX*SEASON"
results.agg[20,2] <- round(AIC(lme15),0)
results.agg[20,3] <- round((nullm.dev - deviance(lme15))/nullm.dev,2)
# #summary(lme14)

#
lme19 <- lm(LOGWT~AGE + MATURITY + SEASON+SEX+COHORT+LOCATION+YEAR + SEASON*YEAR,data = cod.data.1)
results.agg[21,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+LOC+COHORT+YEAR+SEAS/YEAR"
results.agg[21,2] <- round(AIC(lme19),0)
results.agg[21,3] <- round((nullm.dev - deviance(lme19))/nullm.dev,2)
#summary(lme19)
#plot(lme19)

# 
lme23 <- lm(LOGWT~AGE + MATURITY+ SEASON+SEX+COHORT+ LOCATION + YEAR + MATURITY*AGE, data = cod.data.1)
results.agg[22,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+COHORT+LOC+YEAR+MATURITY*AGE"
results.agg[22,2] <- round(AIC(lme23),0)
results.agg[22,3] <- round((nullm.dev - deviance(lme23))/nullm.dev,2)
#summary(lme23)

#l
lme20 <- lm(LOGWT~AGE +MATURITY + SEX+ LOCATION + SEASON + YEAR + COHORT+SEX*AGE, data = cod.data.1)
results.agg[23,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+COHORT+LOC+YEAR+SEX*AGE"
results.agg[23,2] <- round(AIC(lme20),0)
results.agg[23,3] <- round((nullm.dev - deviance(lme20))/nullm.dev,2)
summary(lme20)
#plot(lme20)


#did this one because of original plots showing some ages w a time trend
lme22 <- lm(LOGWT~AGE + MATURITY + SEX + SEASON + COHORT + LOCATION + YEAR + AGE*YEAR, data = cod.data.1)
results.agg[24,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+COHORT+LOC+YEAR+YEAR*AGE"
results.agg[24,2] <- round(AIC(lme22),0)
results.agg[24,3] <- round((nullm.dev - deviance(lme22))/nullm.dev,2)
coef(summary(lme22))
anova(lme22)

#logwt~age+ year +season w all interactions 
lme28 <- lm(LOGWT~AGE + MATURITY + SEASON + SEX + COHORT+LOCATION + YEAR + LOCATION*YEAR, data = cod.data.1)
results.agg[25,1] <- "LOGWT~AGE+MATURITY+SEASON+SEX+COHORT+LOC+YEAR+YEAR*AGE"
results.agg[25,2] <- round(AIC(lme28),0)
results.agg[25,3] <- round((nullm.dev - deviance(lme28))/nullm.dev,2)
#summary(lme28)
#anova(lme28)
#plot(lme20)

write.csv(results.agg,file="results.agg.csv",row.names=FALSE,quote=FALSE)
# #data exploration for sex and maturity
# head(cod.data.1)
# boxplot(LOGWT~SEX, data = cod.data.1)
# boxplot(LOGWT~MATURITY, data = cod.data.1)
#just for fun
#plot(K~LOGWT, data=cod.data.1)
