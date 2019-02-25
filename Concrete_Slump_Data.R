data<-Concrete_Slump_Test_Data
data <- data[,c(-1,-12,-13,-14)]
library(car)
scatterplotMatrix(data,spread=FALSE,lty.smooth=2,main="ScatterPlotMatrix of Concrete Slump Data")
cor(data)
summary(data)
colnames(data)[7]<-"Fine_Aggregate"
colnames(data)[6]<-"Coarse_Aggregate"
fit1 <- lm(Slump~Slag+Water+SP+Fine_Aggregate+Coarse_Aggregate, data=data)
summary(fit1)
colnames(data)[3]<-"Fly_Ash"
colnames(data)[9]<-"Slump_Flow"
colnames(data)[2]<-"Slag"
fit2 <- lm(Slump_Flow ~ Cement+Slag+Fly_Ash+Water+SP+Coarse_Aggregate+Fine_Aggregate,data=data)
summary(fit2)
colnames(data)[9]<-"x28_day_Compressive_Strength"
fit3 <- lm(x28_day_Compressive_Strength~Cement+Slag+Fly_Ash+Water+SP+Coarse_Aggregate+Fine_Aggregate,data=data)
summary(fit3)
confint(fit1)
par(mfrow=c(2,2))
plot(fit1)
par(mfrow=c(1,1))
qqPlot(fit1,labels=row.names(data),id.method="identity",simulate=TRUE,main="Q-Q Plot of Slumpfit")
durbinWatsonTest(fit1)
crPlots(fit1)
ncvTest(fit1)
spreadLevelPlot(fit1)
library(gvlma)
gvmodelslumpfit <- gvlma(fit1)
summary(gvmodelslumpfit)
vif(fit1)
sqrt(vif(fit1)) >2
outlierTest(fit1)
hat.plot <- function(fit1) {
    p <- length(coefficients(fit1))
    n <- length(fitted(fit1))
    plot(hatvalues(fit1),main="Index Plot of Hat Values")
    abline(h=c(2,3)*p/n, col="red",lty=2)
    identify(1:n,hatvalues(fit1),names(hatvalues(fit1)))
    }
hat.plot(fit1)
cutoff <-4/(nrow(data)-length(fit1$coefficients)-2)
plot(fit1,which=4,cook.levels=cutoff)
abline(h=cutoff,lty=2,col="red")
avPlots(fit1,ask=FALSE,onepage=TRUE,id.method="identify")
influencePlot(fit1,id.method="identify",main="Influence Plot",sub="Circle size is proportional to Cook's distance")
newSlump2fit <- lm(Slump^2~Water+SP+Slag+Fine_Aggregate+Coarse_Aggregate,data=data[c(-14,-23,-49,-69,-8),])
summary(newSlump2fit)
newSlumpfit <- lm(Slump~Water+Fine_Aggregate,data=data[c(-14,-23,-49,-69,-8),])
Anova(newSlumpfit,fit1)
library(MASS)
stepAIC(fit1)
library(leaps)
leaps <- regsubsets(Slump~Slag+Water+Fine_Aggregate+Coarse_Aggregate+SP,data=data,nbest=4)
plot(leaps,scale="adjr2")
library(car)
subsets(leaps,statistic="cp",main="CP plot for All subsets Regression")
abline(1,1,lty=2,col="red")
