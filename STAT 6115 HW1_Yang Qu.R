library(ISLR)
library(splines)

#import Wage data 
data(Wage)
head(Wage)
dim(Wage)
names(Wage)
attach(Wage) # need to run to attach the data into the current working environment
W1=summary(Wage)
apply(Wage,class)
agelims=range(Wage)
age.grid=seq(from=agelims[1], to=agelims[2])


# use the dataset to draw the graph 
# Fit a natural spline with four degrees of freedom with function "ns" #
fit2 = lm(wage~ns(age, df=4), data=Wage)
pred=predict(fit2, newdata=list(age=age.grid), se=T)

par(mfrow = c(1, 3))
#Plot Wage against to Age
plot(x=age,y=wage,col="gray")
fit2 = lm(wage~ns(age, df=4), data=Wage)
class(Wage$age)
agelims=range(Wage$age)
age.grid=seq(from=agelims[1], to=agelims[2])
pred=predict(fit2, newdata=list(age=age.grid), se=T)
lines(age.grid,pred$fit,lwd=2, col="purple")

#Plot Wage against to Year
plot(x=year,y=wage,col="gray")
fit2 = lm(wage~ns(year, df=4), data=Wage)
class(Wage$year)
yearlims=range(Wage$year)
year.grid=seq(from=yearlims[1], to=yearlims[2])
pred=predict(fit2, newdata=list(year=year.grid), se=T)
lines(year.grid,pred$fit,lwd=2, col="purple")


#Plot Wage against education level presented as boxplot
head(Wage)
Wage$education
class(Wage$education)
list(Wage$education)
label=c("1","2","3","4","5")
myColors=c("deepskyblue","green","yellow","blue","sienna2")
boxplot(Wage$wage ~ Wage$education , col=myColors, ylab="Wage" ,
  xlab="Education Level",names=label)


"test test "









