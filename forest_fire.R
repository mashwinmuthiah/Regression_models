summary(fire_data)
data<-fire_Data
data<-data[,-c(3,4)]
library(car)
cor(data)
data$logarea<- log(data$Area+1)
str(data)
scatterplotMatrix(data,spread = FALSE,lty.smooth=2,main="scatter plot")
fire_fit1<-lm(logarea~FFMC+DMC+Temp+Wind+RH+ISI,data=data)
summary(fire_fit1)
fire_fit2 <- lm(logarea~ISI+Wind+Temp+RH,data=data)
summary(fire_fit2)
par(mfrow=c(2,2))
plot(fire_fit1)
confint(fire_fit1)
confint(fire_fit2)
par(mfrow=c(1,1))
qqPlot(fire_fit1,labels=row.names(data),id.method="identity",simulate=TRUE,main="Q-Q plot of Fit1")
durbinWatsonTest(fire_fit1)
crPlots(fire_fit1)
ncvTest(fire_fit1)
library(gvlma)
spreadLevelPlot(fire_fit1)
gvmodel1 <- gvlma(fire_fit1)
summary(gvmodel1)
vif(fire_fit1)
sqrt(vif(fire_fit1))>2
outlierTest(fire_fit1)
hat.plot <- function(fire_fit1) {p <- length(coefficients(fire_fit1))
  n<-length(fitted(fire_fit1))
  plot(hatvalues(fire_fit1),main="Index plot of hatvalues")
  abline(h=c(2,3)*p/n,col="red",lty=2)
  identify(1:n,hatvalues(fire_fit1),names(hatvalues(fire_fit1)))
  }
hat.plot(fire_fit1)
cutoff <- 4/(nrow(fire_Data)-length(fire_fit1$coefficients-2))
plot(fire_fit1,which=4,cook.levels=cutoff)
abline(h=cutoff,lty=2,col="red")
avPlots(fire_fit1,ask=FALSE,onepage=TRUE,id.method="identify")
influencePlot(fire_fit1,id.method="identify",main="Influence Plot",sub="Circ
le size is proportional to Cook's Distance")
Anova(fire_fit2,fire_fit1)
AIC(fire_fit2,fire_fit1)
library(MASS)
stepAIC(fire_fit1)
library(leaps)
leaps <- regsubsets(logarea~FFMC+DMC+Temp+Wind+RH+ISI,data=data,nbest=4)
plot(leaps,scale="adjr2")
subsets(leaps,statistic="cp",main="CP plot for All subsets Regression")
abline(1,1,lty=2,col="red")
