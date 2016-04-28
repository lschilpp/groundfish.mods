#Liberty Schilpp
#Cod Data preliminary Models - age 1
#11/10/15
#data manipulation section:
cod.data <- read.csv("~/Desktop/MS Thesis/cod_age_data.csv")
#add cohort 
cod.data$COHORT = cod.data$YEAR - cod.data$AGE
#to get only the weight data (post 1992)
cod.wt.data <- cod.data[cod.data$YEAR>=1992,]
#add logweight
cod.wt.data$LOGWT = log(cod.wt.data$INDWT)
#add mean weight for age 1
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
#install.packages("lme4")
library(lme4)
#install.packages("xtable")
#library(xtable)
#subset data into ages:
cod.data.1 <- cod.wt.data[cod.wt.data$AGE==1,];
#nullm=lm(LOGWT~1, data=cod.data.1)
#logweight by year
lme1.w<-lm(LOGWT+YEAR, data=cod.data.1)
plot(LOGWT~YEAR, data=cod.data.1)
abline(lme1.w)
#set up the results table
results <- matrix (NA, nrow = 9, ncol = 5)
AIC(lme1.w)
#logweight by season
lme2.w<-glm(LOGWT~1+SEASON, data=cod.data.1)
summary(lme2.w)
plot(LOGWT~SEASON, data = cod.data.1)
abline(lme2.w)
#logweight by cohort#how do we put the delay on here
#this will be the same as year
#logweight by LOCATION
lme3.w<-glm(LOGWT~1+LOCATION, data = cod.data.1)
summary(lme3.w)
plot(LOGWT~LOCATION, data = cod.data.1)
abline(lme3.w)
#logweight by year and season
lme4.w<-glm(LOGWT~1+YEAR+SEASON, data=cod.data.1)
summary(lme4.w)
#logwt by year and location
lme5.w<-glm(LOGWT~1+YEAR+LOCATION, data=cod.data.1)
summary(lme5.w)
#logwwt by season and location
lme6.w<-glm(LOGWT~1+LOCATION+SEASON, data=cod.data.1)#sofar best AIC
summary(lme6.w)
nullm=glm(LOGWT~1, data=cod.data.1)
#K by year
lme7.w<-glm(K~1+YEAR, data=cod.data.1)
plot(K~YEAR, data=cod.data.1)
abline(lme7.w)
summary1<-xtable(summary(lme7.w))
print.xtable(summary7, type="html", file="summary7.html")
#K by season
lme8.w<-glm(K~1+SEASON, data=cod.data.1)
summary(lme8.w)
plot(K~SEASON, data = cod.data.1)
abline(lme8.w)
#K by cohort#how do we put the delay on here
#this will be the same as year
#K by LOCATION
lme9.w<-glm(K~1+LOCATION, data = cod.data.1)
summary(lme9.w)
plot(K~LOCATION, data = cod.data.1)
abline(lme9.w)
#K by year and season
lme10.w<-glm(K~1+YEAR+SEASON, data=cod.data.1)
summary(lme10.w)
#K by year and location
lme11.w<-glm(K~1+YEAR+LOCATION, data=cod.data.1)
summary(lme11.w)
#K by season and location
lme12.w<-glm(K~1+LOCATION+SEASON, data=cod.data.1)
summary(lme12.w)