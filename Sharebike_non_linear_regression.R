rm(list=ls())
setwd("Desktop/ECON3005")
#load data
library(readr)
df <- read_csv("daydata.csv")

#regression contain all variables
m1 <- lm(cnt~season+yr+mnth+holiday+weekday+workingday+
          temp+atemp+hum+windspeed, data = df)
r1 <- summary(m1)
r1
#significance level 99.9%: season yr weekday weathersit windspeed
#significance level 99%: hum
#significance level 95%: mnth holiday atemp
#not statistic significant: workingday temp

#firstly focus on variables which is not statistic significant
m2 <- lm(cnt~holiday+weekday+workingday+
           temp+atemp+hum+windspeed, data = df)
r2 <- summary(m2)
r2
#workingday: if day is neither weekend nor holiday is 1, otherwise is 0.
#almost equal to the combination of weekday and holiday

#try to use the workingday to replace the weekday and holiday
m3 <- lm(cnt~workingday+
           temp+atemp+hum+windspeed, data = df)
r3 <- summary(m3)
r3
#try to keep weekday and holiday but delete workingday
m4 <- lm(cnt~holiday+weekday+
           temp+atemp+hum+windspeed, data = df)
r4 <- summary(m4)
r4
#compare the m3 and m4
library(stargazer)
stargazer(m2,m3,m4,
          type="text", title="combined Table",
          style="aer", summary=TRUE, align=TRUE,
          no.space=TRUE,
          ord.intercepts=TRUE)

#choose m4

#temp:Normalized temperature in Celsius
#similar to the variable atemp:Normalized feeling temperature in Celsius.
#try to delete the temp just keep atemp
m5 <- lm(cnt~holiday+weekday+
           atemp+hum+windspeed, data = df)
r5 <- summary(m5)
r5

stargazer(m4,m5,
          type="text", title="combined Table",
          style="aer", summary=TRUE, align=TRUE,
          no.space=TRUE,
          ord.intercepts=TRUE)
anova(m4,m5)
#atemp more significant and the regression fit almost same after deleteing
#so we can consider temp as a redundant variable 
#now we keep m4
library(car)
crPlots(m5)

#the distribution of atemp are more likely to fit a log-curve
#try to change the atemp variable into log(atemp)
m6 <- lm(cnt~holiday+weekday+
           log(atemp)+hum+windspeed, data = df)
r6 <- summary(m6)
r6
anova(m5,m6)
stargazer(m5,m6,
          type="text", title="combined Table",
          style="aer", summary=TRUE, align=TRUE,
          no.space=TRUE,
          ord.intercepts=TRUE)

crPlots(m6)

#summary
stargazer(m2,m3,m4,m5,m6,
          type="text", title="combined Table",
          style="aer", summary=TRUE, align=TRUE,
          no.space=TRUE,
          ord.intercepts=TRUE)

A <- matrix(c(3,-1,-2, 1,-3,4, -1,-3,-4, 3,1,2),nrow=4)
A
B <- matrix(c(5,-1,0,-8),nrow=4)
B
solve(A,B)
