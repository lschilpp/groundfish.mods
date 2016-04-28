#This is preliminary plots of weight at each age up to age 11 with spring and fall included
#looking at the mean ind weight at age by yearas well as boxplots to show variation
cod.data <- read.csv("~/Desktop/MS Thesis/cod_age_data.csv")
#View(cod.data);
#to get only the weight data (post 1992)
cod.wt.data <- cod.data[cod.data$YEAR>=1992,]

# need to subset cod.data - for example by age 1
cod.data.1 <- cod.wt.data[cod.wt.data$AGE==1,];
mean.wt.1 <- aggregate(cod.data.1$INDWT,by=list(Year=cod.data.1$YEAR,Age=cod.data.1$AGE),FUN=mean,na.rm=TRUE);
head (mean.wt.1)
head(cod.data.1);
boxplot(INDWT~YEAR, data = cod.data.1,xlab='YEAR',ylab='',axes = "FALSE")
par(new=TRUE)
lines(mean.wt.1$Year,mean.wt.1$x,col="blue", type ="b",lwd = 2)

#par(new=TRUE)
#plot(x=mean.wt.1$Year, y=mean.wt.1$x, type="b", col="2",xlab="YEAR",ylab="MEAN WEIGHT-AGE 1",lwd = 2)
#plot(INDWT~LENGTH, data = cod.data.1)

#for age 2
cod.data.2 <- cod.wt.data[cod.wt.data$AGE==2,]
head(cod.data.2);
#plot(x=cod.data.2$YEAR, y=cod.data.2$INDWT)
boxplot(INDWT~YEAR, data = cod.data.2,xlab='YEAR',ylab='',axes = "FALSE")
mean.wt.2 <- aggregate(cod.data.2$INDWT,by=list(Year=cod.data.2$YEAR,Age=cod.data.2$AGE),FUN=mean,na.rm=TRUE);
par(new=TRUE)
plot(x=mean.wt.2$Year, y=mean.wt.2$x, type="b", lwd = 2, col="6",xlab="YEAR",ylab="MEAN WEIGHT -AGE 2")
#To look at the length weight relationship
plot(INDWT~LENGTH, data = cod.data.2, xlab="LENGTH - AGE 2")

#for age 3
cod.data.3 <- cod.wt.data[cod.wt.data$AGE==3,]
head(cod.data.3)
#plot(x=cod.data.3$YEAR, y=cod.data.3$INDWT)
boxplot(INDWT~YEAR, data = cod.data.3,xlab='YEAR',ylab='',axes = "FALSE")
par(new=TRUE)
mean.wt.3 <- aggregate(cod.data.3$INDWT,by=list(Year=cod.data.3$YEAR,Age=cod.data.3$AGE),FUN=mean,na.rm=TRUE)
head (mean.wt.3)
plot(x=mean.wt.3$Year, y=mean.wt.3$x, type="b", col="6",xlab="YEAR",ylab="MEAN WEIGHT -AGE 3", lwd = 2)

#for age 4
cod.data.4 <- cod.wt.data[cod.wt.data$AGE==4,];
head(cod.data.4);
boxplot(INDWT~YEAR, data = cod.data.4,xlab='',ylab='', axes = FALSE)
#plot(x=cod.data.4$YEAR, y=cod.data.4$INDWT)
par(new=TRUE)
mean.wt.4 <- aggregate(cod.data.4$INDWT,by=list(Year=cod.data.4$YEAR,Age=cod.data.4$AGE),FUN=mean,na.rm=TRUE)
head (mean.wt.4)
plot(x=mean.wt.4$Year, y=mean.wt.4$x, type="b", col="BLUE",xlab="YEAR",ylab="MEAN WEIGHT -AGE 4", lwd = 2)

#for age 5
cod.data.5 <- cod.wt.data[cod.wt.data$AGE==5,];
head(cod.data.5);
boxplot(INDWT~YEAR, data = cod.data.5,xlab='',ylab='', axes = "FALSE")
#plot(x=cod.data.5$YEAR, y=cod.data.5$INDWT)
mean.wt.5 <- aggregate(cod.data.5$INDWT,by=list(Year=cod.data.5$YEAR,Age=cod.data.5$AGE),FUN=mean,na.rm=TRUE);
head (mean.wt.5)
par(new=TRUE)
plot(x=mean.wt.5$Year, y=mean.wt.5$x, type="b", col="CYAN",xlab="YEAR",ylab="MEAN WEIGHT -AGE 5", lwd = 2)


#for age 6
cod.data.6 <- cod.wt.data[cod.wt.data$AGE==6,];
mean.wt.6 <- aggregate(cod.data.6$INDWT,by=list(Year=cod.data.6$YEAR,Age=cod.data.6$AGE),FUN=mean,na.rm=TRUE);
head(cod.data.6);
boxplot(INDWT~YEAR, data = cod.data.6,xlab='',ylab='', axes = "FALSE")
par(new=TRUE)
plot(x=mean.wt.6$Year, y=mean.wt.6$x, type="b", col="PURPLE",xlab="YEAR",ylab="MEAN WEIGHT -AGE 6", lwd = 2)
#plot(x=cod.data.6$YEAR, y=cod.data.6$INDWT)

#for age 7
cod.data.7 <- cod.wt.data[cod.wt.data$AGE==7,];
mean.wt.7 <- aggregate(cod.data.7$INDWT,by=list(Year=cod.data.7$YEAR,Age=cod.data.7$AGE),FUN=mean,na.rm=TRUE)
head(cod.data.7);
plot(x=cod.data.7$YEAR, y=cod.data.7$INDWT)
boxplot(INDWT~YEAR, data = cod.data.7,xlab='',ylab='', axes = "FALSE")
par(new=TRUE)
plot(x=mean.wt.7$Year, y=mean.wt.7$x, type="b", col="GREEN",xlab="YEAR",ylab="MEAN WEIGHT -AGE 7", lwd = 2)

#for age 8
cod.data.8 <- cod.wt.data[cod.wt.data$AGE==8,];
mean.wt.8 <- aggregate(cod.data.8$INDWT,by=list(Year=cod.data.8$YEAR,Age=cod.data.8$AGE),FUN=mean,na.rm=TRUE)
head(cod.data.8);
plot(x=cod.data.8$YEAR, y=cod.data.8$INDWT)
boxplot(INDWT~YEAR, data = cod.data.8,xlab='',ylab='', axes = "FALSE")
par(new=TRUE)
plot(x=mean.wt.8$Year, y=mean.wt.8$x, type="b", col="ORANGE",xlab="YEAR",ylab="MEAN WEIGHT -AGE 8", lwd = 2)

#for age 9
cod.data.9 <- cod.wt.data[cod.wt.data$AGE==9,];
mean.wt.9 <- aggregate(cod.data.9$INDWT,by=list(Year=cod.data.9$YEAR,Age=cod.data.9$AGE),FUN=mean,na.rm=TRUE)
head(cod.data.9);
plot(x=cod.data.9$YEAR, y=cod.data.9$INDWT)
boxplot(INDWT~YEAR, data = cod.data.9,xlab='',ylab='', axes = "FALSE")
par(new=TRUE)
plot(x=mean.wt.9$Year, y=mean.wt.9$x, type="b", col="CYAN",xlab="YEAR",ylab="MEAN WEIGHT -AGE 9", lwd = 2)

#for age 10
cod.data.10 <- cod.wt.data[cod.wt.data$AGE==10,];
mean.wt.10 <- aggregate(cod.data.10$INDWT,by=list(Year=cod.data.10$YEAR,Age=cod.data.10$AGE),FUN=mean,na.rm=TRUE)
head(cod.data.10);
plot(x=cod.data.10$YEAR, y=cod.data.10$INDWT)
boxplot(INDWT~YEAR, data = cod.data.10,xlab='',ylab='', axes = "FALSE")
par(new=TRUE)
plot(x=mean.wt.10$Year, y=mean.wt.10$x, type="b", col="PINK",xlab="YEAR",ylab="MEAN WEIGHT -AGE 10", lwd = 2)

#to look at the mean weight by year for ages 1-5 only - looks "busy"
lines(mean.wt.1$Year,mean.wt.1$x,col="blue", type ="b",lwd = 2, xlim = c(1992 - 2015), ylim=c(1,12))
lines(mean.wt.2$Year,mean.wt.2$x,col="red", type ="b", lwd = 2)
lines(mean.wt.3$Year,mean.wt.3$x,col="green", type ="b", lwd = 2)
lines(mean.wt.4$Year,mean.wt.4$x,col="cyan", type ="b", lwd = 2)
lines(mean.wt.5$Year,mean.wt.5$x,col="orange", type ="b", lwd = 2)
#plot(x=mean.wt.1$Year, y=mean.wt.1$x, type="b", col="red",xlab="YEAR",ylab="MEAN WEIGHT", lwd = 2)
#par(new=TRUE)
#plot(x=mean.wt.2$Year, y=mean.wt.2$x, type="b", col="GREEN",xlab="YEAR",ylab="MEAN WEIGHT",lwd = 2, axes = FALSE)
#par(new=TRUE)
#plot(x=mean.wt.3$Year, y=mean.wt.3$x, type="b", col="orange",xlab="YEAR",ylab="MEAN WEIGHT",lwd = 2, axes = FALSE)
#par(new=TRUE)
#plot(x=mean.wt.4$Year, y=mean.wt.4$x, type="b", col="BLUE",xlab="YEAR",ylab="MEAN WEIGHT", lwd = 2, axes = FALSE)
#par(new=TRUE)
#plot(x=mean.wt.5$Year, y=mean.wt.5$x, type="b", col="CYAN",xlab="YEAR",ylab="MEAN WEIGHT", lwd = 2, axes = FALSE) 
#legend("topright", title = "AGES", c("Age 1","Age 2","Age 3","Age 4","Age 5"),col=c("red","GREEN","orange","BLUE","CYAN"))


#to look at the mean weight by year for ages 6 - 11 (still looks busy)

lines(mean.wt.6$Year,mean.wt.6$x,col="blue", type ="b",lwd = 2, ylim=c(1,12))
lines(mean.wt.7$Year,mean.wt.7$x,col="red", type ="b", lwd = 2)
lines(mean.wt.8$Year,mean.wt.8$x,col="green", type ="b", lwd = 2)
lines(mean.wt.9$Year,mean.wt.9$x,col="cyan", type ="b", lwd = 2)
lines(mean.wt.10$Year,mean.wt.10$x,col="orange", type ="b", lwd = 2)

legend(title = "AGES", c("Age 6","Age 7","Age 8","Age 9","Age 10"),col=c("blue","red","green","CYAN","orange"))
#for age 11 ---- not enough data to use age 11 fish
#cod.data.11 <- cod.wt.data[cod.wt.data$AGE==11,];
#mean.wt.11 <- aggregate(cod.data.11$INDWT,by=list(Year=cod.data.11$YEAR,Age=cod.data.11$AGE),FUN=mean,na.rm=TRUE)
#head(cod.data.11);
#plot(x=cod.data.11$YEAR, y=cod.data.11$INDWT)
#boxplot(INDWT~YEAR, data = cod.data.11,xlab='',ylab='', axes = "FALSE")
#par(new=TRUE)
#plot(x=mean.wt.11$Year, y=mean.wt.11$x, type="b", col="GREEN",xlab="YEAR",ylab="MEAN WEIGHT -AGE 11", lwd = 2)

#LENGTH AS A FACTOR OF YEAR
lm1<-lm(LENGTH~YEAR, data=cod.data.1)
plot(x=cod.data.1$YEAR, y=cod.data.1$LENGTH, xlab = "year", ylab = "length")
abline=lm1
summary(lm1)
#Length as a factor of season
plot(x=cod.data.1$SEASON, y=cod.data.1$LENGTH)
lm2<-lm(LENGTH~SEASON,data=cod.data.1);
summary(lm2);
#LEnght as a factor of STRATUM
lm3<-lm(LENGTH~STRATUM, data=cod.data.1)
plot(x=cod.data.1$STRATUM, y=cod.data.1$LENGTH);#need season,location as a factor
#Weight as a factor of year
lm1.w<-lm(INDWT~YEAR, data=cod.wt.data.1)
plot(lm1.w)
#weight as a factor of stratum, in 2 year olds
cod.data.2 <- cod.wt.data[cod.wt.data$AGE==2,]
head(cod.data.2);
lm1.w.2<-lm(INDWT~STRATUM, data=cod.wt.data.2)
plot(INDWT~STRATUM, data=cod.data.2)
