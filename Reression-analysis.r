library(DBI) #coverts r to 
library(RSQLite) 
library(data.table)
setwd("D:/Roshni/UTD/BUAN 6356 Business Analytics with R")

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
###############      1##################################
con <- dbConnect(SQLite(),'wooldridge2.db')
wage1 <- dbReadTable(con,'wage1')
summary(wage1)
#Avg education: 12.5 years,min: 0 years, max: 18 years
###########################################################
avg_wage <- mean(wage1$wage) #Average wage = 5.9
min(wage1$wage) #Minimum wage value = 0.53
max(wage1$wage) #Maximum wage value = 25
#Since the maximum value is 25 the average value close to 6 seems low
###########################################################
#Cpi = Cost of goods and services at current year prices/ Cost of goods and services at base year
#Economic report of the president says CPI values of 1976 and 2010 are 56.9 and 184.0
CPI <- 184.0/56.9 #3.233 is the CPI value
###########################################################
avg_wage <- mean(wage1$wage) # Average wage of 1976 is 5.9089
CPI_2010 <- (CPI*avg_wage)   # Average wage of 2019 is 19.1081
#This wage amount is closer to the max wage value (25) and so it seems reasonable
###########################################################
library(plyr)
count(wage1$female,1)
# x freq
# 0  274
# 1  252
#Male population = 274 and Female population = 252
###############                    2##################################
meap01 <- dbReadTable(con,'meap01')
summary(meap01)
#Min:0.00,Max:100.00 of math4 The range shows the difference between max 
#                   value and minimum value and so makes sense
##########################################################
library(formattable)
length(which(meap01$math4==100)) #38 schools have a 100 % pass rate 
max(meap01$index)
percent  <- (38/1822)* 100 #2.08562% of students have a perfect pass rate
##########################################################
length(which(meap01$math4==50))
#Schools having math pass rates of exactly 50% = 17
##########################################################
summary(meap01$math4)
summary(meap01$read4)
mean(meap01$read4)
mean(meap01$math4)
sd(meap01$math4)
sd(meap01$read4)
# mean(meap01$read4) = 60.06188, mean(meap01$math4) = 71.909
# sd(meap01$math4) = 19.95409, sd(meap01$read4) = 19.14729
# Since the deviation of math is more there are more chances to fail in math
##########################################################
cor(meap01$math4,meap01$read4)
#Correlation of math and reading is 0.8427281. This value is close to 
#positive 1 and so they have a strong relation with each other
##########################################################
mean(meap01$exppp)    #5194.865
sd(meap01$exppp)      #1091.89
var(meap01$exppp)     #1192223
# The variance is quiet high in the expense per pupil spending
###########################################################
100 * (log(6000)-log(5500)) #8.701138
#(Increase or decrease in rate/Original rate) * 100
((500/5500) * 100)            #A exceeds B by 9.090909
#The values differ by 9.090909 - 8.701138 = 0.389771
#####################                     3############################
pension_Plan <- dbReadTable(con,'401k')
mean(pension_Plan$prate)   #87.36291
mean(pension_Plan$mrate)   #0.7315124
###########################################################
linearMod <- lm(prate~mrate,data = pension_Plan)
summary(linearMod)
#R-Squared value = 0.0747, (prate = 83.1 + 5.9(mrate))
summary(pension_Plan) #Sample size = 1534
###########################################################
linearMod <- lm(prate~mrate,data = pension_Plan) #Intercept = 83.075 Coeff of mrate = 5.8611
#The intercept in the equation will become the prate 
#if mrate is 0 prate = 83.1%
#Even if mrate is equal to 1 the prate value increases only by a factor of 5.9 
###########################################################
#PRATE = 83.1 + 5.9(MRATE)
#If mrate = 3.5
prate1 <- (83.1 + 5.9 * (3.5))  
prate1  # 103.75
#The regression has given a strange value. The participation rate, in percent 
#has crossed the 100% scale
############################################################
#R squared value = 0.0747 meaning mrate contributes to only 7.4 % of the variation in prate
###############                     4###################################
ceosal2 <- dbReadTable(con,'ceosal2')
mean(ceosal2$salary) #865.8644
mean(ceosal2$comten) #Company tenure 22.50282
mean(ceosal2$ceoten) #CEO Tenure 7.954802
############################################################
length(which(ceosal2$ceoten==0)) #5 CEO's are in their 1st year of tenure
max(ceosal2$ceoten) #Longest tenure as a CEO is 37 years
############################################################
lm <- lm(formula = log(salary) ~ ceoten, data = ceosal2) #The salary increase is 11.75%
summary(lm) # salary = 6.5054 + 0.00972(ceoten) 
lm <- lm(formula = (salary) ~ ceoten, data = ceosal2)
summary(lm)
(salary = 772.4 + 11.7(ceoten)) # Every increase in ceoten results in 11.7 % increase in salary
################                     5##################################
wage2 <- dbReadTable(con,'wage2')
mean(wage2$wage) # 957.9455
mean(wage2$IQ)   # 101.2824
sd(wage2$IQ)     # 15.05264
############################################################
lm<-lm(formula = wage~IQ, data = wage2)
summary(lm)
# wage = 116.99 + 8.30*IQ
# If IQ = 15 Wage increases by 124.5(15*8.30), R Squared value = 0.0955
# Only 9.5% of the variation in wage is contributed by IQ
###########################################################
lm <- lm(log(wage)~IQ, data=wage2)
summary(lm) #log(wage)=5.88699+0.00880(IQ)
# If IQ = 15 Wage increases by 0.132(15*0.00880), R Squared value = 0.0990
# Only 9.9% of the increase in wage is contributed by IQ
#########################                     6##########################
meap_math <- dbReadTable(con,'meap93')
cor(meap_math$math10,meap_math$expend) # Cor = 0.1815 (Close to 0)
lm2 <-lm(formula = math10~expend, data = meap_math)
summary(lm2) # R Squared value = 0.0329 shows that the effect of expenditure on math10
             # is only 3.2% and so it would make sense to say it had a diminishig effect 
#############################################################
lm<-lm(formula=math10~log(expend),data = meap_math)
summary(lm) #math10= -69.341 + 11.164*expend
# B1 = 11.164/10 = 1/1164%
# B1 = 11.164 * (10/100) = 1.1164%
# Hence b1/10 is the percentage point change in math10 gives 
# a 10% increase in expend
#############################################################
lm3<-lm(formula=math10~log(expend),data = meap_math)
summary(lm3)
#math10= -69.341 + (11.164*log(expend))
#R squared = 0.02966 
summary(meap_math$index) # Sample size   = 408
##############################################################
lm<-lm(formula=math10~log(expend),data = meap_math)
summary(lm)
#math10 = -69.341 + (11.164*log(expend))
# 11.164 * 10/100 = 1.1164 #The increase in math10 will be 1.1164
##############################################################
max(meap_math$math10) # the max value is 66.7 in the meap93 dataset and so
# even if the fitted value is greater than 100 we do not have to worry
##################                     7##################################
hprice <- dbReadTable(con,'hprice1')
lm<-lm(formula= price~sqrft+bdrms, data = hprice)
summary(lm)  
#price = -19.32 + 0.13(sqrft) + 15.20(bdrms)
##############################################################
# If there is a increase in price for a house with one more bedroom
# The price would increase by 15.19819 (In thousands of $)
##############################################################
# One more bedroom price is 15.19819 and 
# 140*0.12844 = 17.9816 would be the additional sqrft
Tot_Price_inc = 17.9816+15.19819  # Price would increase by 33.17979
#The sqrft change seems to impact the prize on a higher factor
##############################################################
lm<-lm(formula= price~sqrft+bdrms, data = hprice)
summary(lm) # R Squared vaule is 0.6319 so we can say that sqrtft and bdrms 
            #affect 63% of increase in price
##############################################################
#price = -19.315+0.12844(sqrft)+15.19819(bdrms)
Quest5_sols <- (-19.315+(0.12844*2438)+(15.19819*4)) 
# Predicted Price would be 354.6145 in thousands of dollors
#############################################################
Predicted_price = 354614.5
Actual_price = 300000
Residual_price = Predicted_price - Actual_price ###54614.5$
# The buyer has underpaid for this house
#####################                     8############################
ceosal2 <- dbReadTable(con,'ceosal2')
lm <- lm(formula=log(salary)~log(sales)+log(mktval),data = ceosal2)
summary(lm)
#Salary = 4.62092 + 0.016213log(sales) + 0.010671log(mktval) R squared value = 29.91%
##########################################################
lm <- lm(formula=log(salary)~log(sales)+log(mktval)+(profits),data = ceosal2)
summary(lm)
#In log(profits) : NaNs produced error is displayed when log is used on profits
#log(x) will produce NaN any time x is less than zero
# R Squared values is 0.2993 which means only 29.93% of the variation is caused by
# these performance variables
###########################################################
lm <- lm(formula=log(salary)~log(sales)+log(mktval)+(profits)+(ceoten),data = ceosal2)
summary(lm)
#Salary = 4.56+0.16log(sales)+0.1018(mktval)+0.000029(profits)+0.01168(ceoten)
# Salary would increase by 1.168% for another year of CEO tenure
###########################################################
mktval <- log(ceosal2$mktval)
profits <- ceosal2$profits
cor(mktval,profits)   #0.7768976 Fairly close to 1 so they are positively co related
####################                     9#############################
attend <- dbReadTable(con,'attend')
attend$atndrte = (attend$attend)/32
summary(attend)
#atndrte Min:0.0625,Max:1,Mean:0.8171
#priGPA  Min:0.857,Max:3.930,Mean:2.587
#ACT  Min:13,Max:32,Mean:22.51
###########################################################
lm <- lm(formula=atndrte~priGPA+ACT,data = attend)
summary(lm)
#atndrte = 0.75700+0.17261(priGPA)-0.01717(ACT)
#The intercept gives the atendance rate as 75% if priGPA and ACT are constant 
###########################################################
lm <- lm(formula=atndrte~priGPA+ACT,data = attend)
summary(lm)
#The slope intercept of priGPA is 0.17261 (17.26%) and the slope of ACT is
#0.01717 (1.717%). The negative relation between number of classes attended and ACT
# score is a surprise
###########################################################
#atndrte = 0.75700+0.17261(priGPA)-0.01717(ACT)
atndrte <- 0.75700+(0.17261*3.65)-(0.01717*20) #1.043627 
# The estimated atndrte is 1.0436. This (104.36%) is not a valid estimate as 
# it has crossed 100%
length(which(attend$priGPA==3.65,attend$ACT==20)) # One student has these values
##########################################################
StudentA <- 0.75700+(0.17261*3.1)-(0.01717*21) #0.931521(93%)
StudentB <- 0.75700+(0.17261*2.1)-(0.01717*26) #0.673061(67%)
Pred_Diff <- StudentA - StudentB               #0.25846(26%)
#####################                     10##########################
htv <- dbReadTable(con,'htv')
summary(htv)
#Range of educ = 14
length(which(htv$educ==12))   #512 
(512/1230) * 100 #41.62602% of the men have finished the 12th grade
#Mean(men) = 13,Mean(mother)= 12.18,Mean(father) = 12.45
# Yes they have a higer education on average 
###########################################################
lm <- lm(formula=educ~motheduc+fatheduc,data = htv)
summary(lm)
#educ=6.96+0.30(motheduc)+0.19(fatheduc)
##R squared value says 24% of increase in educ is contributed by parents education
##Mothers education contributes to 30% rise in education level
##Fathers education contributes to 20% of rise in education level
##Mothers educ contributes more to the increase in education
###########################################################
lm <- lm(formula=educ~motheduc+fatheduc+abil,data = htv)
summary(lm)
#educ=8.45+0.19(motheduc)+0.11(fatheduc)+0.50(abil)
#If the parents education is conrolled abil variable contributes to 50% of
#        increase in education rate which is a significant incease
###########################################################
abil2 <- (htv$abil^2)
lm <- lm(formula=educ~motheduc+fatheduc+abil+abil2,data = htv)
summary(lm)
#educ=8.240226+0.190126(motheduc)+0.108939(fatheduc)+0.401462(abil)+0.050599(abil^2)
#The education values are minimised using derivative methods
# First derivative  
#d educ / d abil = 0.4015 + 2*0.0506abil = 0 
abil = -0.4015/(2*0.0506) # -3.967391
# Second derivative
abil = 2*0.0506 #d educ / d abil = 0.1012
# Thus the minimised value of abil is 3.967391
###############################################################
length(which(htv$abil< -3.967391)) #15
max(htv$index) # 1229
min(htv$index) # 0
(15/1230 ) * 100 #only 1.220504% have the ability less than our previous calculation
#This helps us realise that our sample data needs to include more people in this 
#particular range of ability
###############################################################
htv$motheduc=12.18 #Average values as per                     
htv$fatheduc=12.45
abil2 <- (htv$abil^2)
abil_lm <- lm(formula=educ~motheduc+fatheduc+abil+abil2,data = htv)
summary(lm)
#educ=8.240226+(0.190126*(12.18))+(0.108939*(12.45))+0.401462(abil)+0.050599(abil^2)
#educ = 11.91225+0.401462(abil)+0.050599(abil^2)
ggplot(htv,aes(x=abil,y=educ)) + 
  geom_point() + 
  geom_line(aes(y=predict(abil_lm)),color='red') + 
  scale_x_continuous('Predicted education') +
  scale_y_continuous('Ability')
###############################################################