library(DBI) #coverts r to 
library(RSQLite) 
library(data.table)
library(broom)
library(data.table)
library(DBI)
library(RSQLite)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(broom)
library(sandwich) 
library(tseries)
library(magrittr)
library(stargazer)
library(car)
library(vars) 
library(reshape2)
library(forecast)
library(TSA)
library(data.table)
library(dplyr)
library(DBI)
library(tseries)
library(TSA)
library(vars)
library(ggplot2)
library(gtable)
library(grid)
library(forecast)
setwd("D:/Roshni/UTD/BUAN 6356 Business Analytics with R")
dbDisconnect(con)
con <- dbConnect(SQLite(),'wooldridge2.db')
wpull <- function(tablename){
  con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db')
  dt <- DBI::dbReadTable(con,tablename)
  dt <- data.table(dt)
  print(
    DBI::dbReadTable(
      con,
      paste(tablename,'labels',sep='_')
    )
  )
  DBI::dbDisconnect(con)
  rm(con)
  return(dt)
}
library(ggplot2)
tidy(modela,conf.int=TRUE)
sqrt(diag(vcovHC(modela)))
tidyg <- function(model,vc=vcov(model),conf.int=FALSE,conf.level=0.95){
  dt <- tidy(model,conf.int=conf.int,conf.level=conf.level)
  dt$std.error <- sqrt(diag(vc))
  dt$statistic <- dt$estimate/dt$std.error
  dt$p.value <- 2*pnorm(-abs(dt$statistic))
  if(conf.int){
    dt$conf.low <- dt$estimate+qnorm((1-conf.level)/2)*dt$std.error
    dt$conf.high <- dt$estimate-qnorm((1-conf.level)/2)*dt$std.error
  }
  return(dt)
}
tidyw <- function(model,...){
  return(tidyg(model,vc=sandwich::vcovHC(model),...))
}
##############################################################
#1
wpull <- wpull('mlb1')
mlb1 <- dbReadTable(con,'mlb1')
lm <- lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc
             +allstar+frstbase+scndbase+thrdbase+shrtstop+catcher,data=mlb1)
summary(lm)
#H0= B13 = 0
#The t-ratio on the catcher variable is 1.931 with a two-sided
# p-value of 0.05432 This ratio is significant only at the 10% level.
############################################################
#H0: B9= B10= B11= B12= B13 = 0
#H1: B9<> B10<> B11<> B12<> B13
lm <- lm(salary~frstbase+scndbase+thrdbase+shrtstop+catcher,data=mlb1)
summary(lm)
#Given this joint test, we conclude that there is not a
# significant difference in salaries across nonpitching positions.
#############################################################
#Both tests give the same results that there are other factors affecting the salary 
#than the field position of a player
lm <- lm(formula=salary~years+gamesyr+bavg+hrunsyr+rbisyr
         +runsyr+fldperc+allstar+frstbase+scndbase+
           thrdbase+shrtstop+catcher,data=mlb1)
summary(lm)
 #############################################################
# 2
wpull <- wpull('gpa2')
gpa2 <- dbReadTable(con,'gpa2')
###1###
lm <- lm(formula= colgpa~hsize + I(hsize^2) +hsperc +sat +female +athlete,data= gpa2)
summary(lm)
#Female and hsize seems a little odd in this equation and I would expect little to no effect on gpa
#All other factors should have a possitive value 
#############################################################
lm <- lm(formula= colgpa~hsize + I(hsize^2) +hsperc +sat +female +athlete,data= gpa2)
summary(lm)
#colgpa = 1.241-5.685e-02(hsize)+4.675e-03((hsize^2))-1.321e-02(hsperc)+1.646e-03(sat)
#+1.549e-01(female)+1.693e-01(athlete)+U
model1 <- lm(colgpa~hsize + I(hsize^2) +hsperc +sat +female +(athlete==1),data= gpa2)
summary(model1) # Every one athelete makes an impact of 0.1693% increase in GPA
model2 <- lm(formula= colgpa~hsize + I(hsize^2) +hsperc +sat +female +(athlete==0),data= gpa2)
summary(model2) # Every one athelete makes an impact of 0.1693% decrease in GPA
# In both models the athelete variable is statistically significant
###############################################################
lm3 <- lm <- lm(formula= colgpa~hsize + I(hsize^2) +hsperc +female +athlete,data= gpa2)
summary(lm3)
#Athelete has become insignificant
#R square has lowered
################################################################
model4 <- lm(colgpa~hsize + I(hsize^2) +hsperc +sat +(female ==1)+(athlete==1),data= gpa2)
model5 <- lm(colgpa~hsize + I(hsize^2) +hsperc +sat +(female ==0)+(athlete==1),data= gpa2)
summary(model4)
summary(model5)
##############################################################
model6 <- lm(colgpa~hsize + I(hsize^2) +hsperc +sat +I((female ==1)*(athlete==1))+I((female ==1)*(athlete==0))+I((female ==0)*(athlete==1)),data= gpa2)
summary(model6)
#After solving for Theta
model7 <- lm(colgpa~hsize + I(hsize^2) +hsperc +sat +I((female ==1)*(athlete==1))+(I((female ==1)*(athlete==1) + I((female ==1)*(athlete==0))))+I((female ==0)*(athlete==1)),data= gpa2)
summary(model7)         
#It is insignificant at the 0.1%level but not at the 5% level
###############################################################
model8 <- lm(formula= colgpa~(female==1) +sat,data= gpa2)
summary(model8)
model9 <- lm(formula= colgpa~(female==0) +sat,data= gpa2)
summary(model9)            
#The effect on colgpa is unaffected by sat when gender is differed
###############################################################
# 3
wpull <- wpull('loanapp')
loanapp <- dbReadTable(con,'loanapp')
###############################################################
#After controlling the factors b1 > 0  there is descrimination against other races 
###############################################################
model1 <- lm(formula= approve~white,data= loanapp)
summary(model1)
#approve = 0.70779 + 0.20060(white) 
#This means that there is 20.060 % chance for a white person to get their loan approved
#The T Stat is 10.11 which is quiet high which means that it is statistically significant
###############################################################
model2 <- lm(formula= approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+
               cosign+chist+pubrec+mortlat1+mortlat2+vr,data= loanapp)
summary(model2)
#The chances have gone down to 12% but the significance is still high
#############################################################
model3 <- lm(formula= approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+
               cosign+chist+pubrec+mortlat1+mortlat2+vr+I(white*obrat),data= loanapp)
summary(model3)
#The interaction is highly significant even in the 0.1% level
##############################################################
model4 <- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+
         cosign+chist+pubrec+mortlat1+mortlat2+vr+I(white*(obrat-32)),data=loanapp)
summary(model4)
confint(model4,level = 0.95) # 0.0732462753  0.152430088 in white
##############################################################
# 4
hprice1 <- dbReadTable(con,'hprice1')
model5 <- lm(price~lotsize+sqrft+bdrms,data=hprice1)
tidyw(model5,conf.int= TRUE)
#The std error on bedrooms and intercept look larger than usual
##############################################################
model6 <- lm(log(price)~log(lotsize)+log(sqrft)+bdrms,data=hprice1)
tidyw(model6,conf.int= TRUE)
##############################################################
#The use of log has reduced the heteroskedasticity in the 
#later model also sqrft has become significant in the second model
###############################################################
# 5
gpa1 <- dbReadTable(con,'gpa1')
model1 <- lm(colGPA ~ hsGPA+ACT+skipped+ PC,data =gpa1)
summary(model1) #1.35+0.412(hsGPA)+0.0133(ACT)-0.0710(skipped)+0.12444(PC) OLS
Q5res <- residuals(model1)
Q5prd <- predict(model1)
#Residuals SE 0.32750+0.09243(hsGPA)+0.01044(ACT)+0.02625(skipped)+0.05731(PC)
###############################################################
model2 <- lm(I(Q5res^2)~Q5prd+I(Q5prd^2))
tidyw(model2)
# At the 5% level, we conclude there is evidence of
#heteroskedasticity in the errors of the colGPA equation
summary(model2)
################################################################
gpa1$res <- residuals(model2)
gpa1$res
gpa1$ressq <- gpa1$res^2
gpa1$ressq <-gpa1$res^2
lm<- lm(ressq~colGPA+I(colGPA^2), data = gpa1)
lm
gpa1$hi <- lm$fitted.values
gpa1$hi
gpa1$weight <- 1/gpa1$hi
lm <- lm(colGPA~hsGPA+ACT+skipped+PC,weights=weight,data=gpa1)
summary(lm)
#The R squared value is larger
###############################################################
coeftest(lm,vcov=vcovHC)
#The standard errors have not changed much 
################################################################
