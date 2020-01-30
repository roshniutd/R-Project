library(DBI) 
library(RSQLite) 
library(data.table)
library(broom)
library(tidyverse)
library(plm)
library(margins)
library(sandwich)
library(lmtest)
library(ggplot2)

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
###########################################################
#Question 1
wpull <- wpull('hprice1')
hprice1 <- dbReadTable(con,'hprice1')
lm01 <- step(lm(log(price)~(bdrms+lotsize+sqrft)^2+(bdrms+lotsize+sqrft)^3+I(bdrms^2)+I(lotsize^2)+I(sqrft^2)+colonial, data=hprice1))
summary(lm01,k=log(nrow(hprice1))) 
lm02 <- step(lm(price~(bdrms+lotsize+sqrft)^2+(bdrms+lotsize+sqrft)^3+I(bdrms^2)+I(lotsize^2)+I(sqrft^2)+colonial, data=hprice1))
summary(lm02,k=log(nrow(hprice1)))
BIC(lm(price ~ bdrms + lotsize + sqrft + I(bdrms^2) + I(lotsize^2) + 
         colonial + bdrms:lotsize, data = hprice1)) #952.4733
AIC(lm(price ~ bdrms + lotsize + sqrft + I(bdrms^2) + I(lotsize^2) + 
         colonial + bdrms:lotsize, data = hprice1)) #930.1773
#The R^2 of model 2 is better and so model 2 is a better Model
###########################################################
#Question 2
wpull <- wpull('gpa2')
gpa2 <- dbReadTable(con,'gpa2')
lm04 <- step(lm(colgpa~sat+(tothrs+hsize+hsrank+hsperc)^2+I(sat^2)+I(hsize^2)+I(hsrank^2)+athlete+female+black+white*black, data=gpa2))
summary(lm04,k=log(nrow(gpa2))) #R-Squared - 0.3439
BIC(lm(colgpa ~ sat + tothrs + hsize + hsrank + hsperc + 
         I(sat^2) + I(hsize^2) + I(hsrank^2) + athlete + female + 
         black + tothrs:hsize + tothrs:hsrank + tothrs:hsperc + hsize:hsrank + 
         hsrank:hsperc, data = gpa2)) #6690.507
AIC(lm(colgpa ~ sat + tothrs + hsize + hsrank + hsperc + 
         I(sat^2) + I(hsize^2) + I(hsrank^2) + athlete + female + 
         black + tothrs:hsize + tothrs:hsrank + tothrs:hsperc + hsize:hsrank + 
         hsrank:hsperc, data = gpa2)) #6576.608
#lm4 looks like the comparetivly best model since all other models have an even lower R square
###########################################################
#Question 3
wpull <- wpull('mlb1')
mlb1 <- dbReadTable(con,'mlb1')
lm05 <- step(lm(log(salary)~teamsal+nl+years+games+runs+triples+so+sbases+fldperc+I(frstbase+scndbase+shrtstop+thrdbase)^2+outfield
                  +catcher+yrsallst+hispan+black+gamesyr+I(gamesyr^2)+allstar+rbisyr+percblck+perchisp+blckpb+hispph+whtepw+blckph+hisppb, data=mlb1))
summary(lm05,k=log(nrow(mlb1))) #R-Squared =0.6905
BIC(lm(log(salary) ~ nl + years + games + runs + triples + 
         so + I(frstbase + scndbase + shrtstop + thrdbase) + yrsallst + 
         hispan + black + gamesyr + allstar + rbisyr + percblck + 
         perchisp + blckpb + hispph + whtepw + blckph + hisppb, data = mlb1)) #775.8001
AIC(lm(log(salary) ~ nl + years + games + runs + triples + 
         so + I(frstbase + scndbase + shrtstop + thrdbase) + yrsallst + 
         hispan + black + gamesyr + allstar + rbisyr + percblck + 
         perchisp + blckpb + hispph + whtepw + blckph + hisppb, data = mlb1)) #692.22
#The R Squared of this model is the highest and the BIC value also looks good comparetively
#lm05 is the best model even though there are many unexplained characters to the model
###########################################################
#Question 4.A
wpull <- wpull('rental')
rental <- dbReadTable(con,'rental')
y90.dummy=rep(0,nrow(rental))
y90.dummy[rental$year==90]=1
y90.dummy
pctstu <- ((rental$enroll/rental$pop)* 100)
rental2 <- pdata.frame(rental,index = c("city","year"),drop.index = TRUE)
lm1 <- plm(log(rent)~y90.dummy +log(pop)+log(avginc)+pctstu,model="pooling", data=rental2)
summary(lm1)
#log(rent)=-0.569+0.262y90+0.041log(pop)+0.571log(avgincit)+0.0050pctstu
#R Squared = 0.86128 and N = 128 and B3= 0.571
############################################################
#4.B
#The errors seem normal but since we have not taken two different errors for the 
#different time periods they are not valid.
############################################################
#4.C
lm2 <- plm(log(rent)~y90.dummy+log(pop)+log(avginc)+pctstu, model="fd", data=rental)
summary(lm2)
#log(rent) = 0.3855y90 + 0.0722log(pop) + 0.3099log(avginc) + 0.0112033 pctstu
#The B3 value has doubled from the previos model after first differencing
#############################################################
#4.D
lm3 <- plm(log(rent)~y90.dummy+log(pop)+log(avginc)+pctstu,model="within", data=rental2)
summary(lm3)
#The model is different from the previous model in R squared and significance
###############################################################
###############################################################
#Question 5.A
wpull <- wpull('murder')
murder <- dbReadTable(con,'murder')
murder2 <- pdata.frame(murder,index = c("state","year"))
lm1 <- plm(mrdrte~year+exec+unem,model="pooling", data=murder2)
summary(lm1)
#The B1 unemployement should increase when there is a rise in murder rate this is 
#my expectation
################################################################
#5.B
lm4 <- plm(mrdrte~year+exec+unem,model="pooling", data=murder2)
summary(lm4)
#There is no statistically significant deterrant effect
###############################################################
#5.C
lm3 <- plm(mrdrte~as.factor(year)+exec+unem,model="fd", data=murder2 %>% subset(year==90 | year==93))
summary(lm3,vcovHC)#mrdrte=0.4-0.1038(exec)-0.06659(unem)
#The coeffeciant on exec is highly significant and it seems to have a deterrant effect 
###############################################################
#5.D
lm5 <- plm(mrdrte~as.factor(year)+exec+unem,model="fd", data=murder2)
summary(lm5)
#There is no heteroskedascity present in the data
################################################################
#5.E
murder1993 <- murder %>% subset(year==93)
(murder$exec < 22)
max(murder$exec)
murder <- data.table(murder)
murder4 <- murder1993[, lapply(.SD[], sum), by = list(exec, state)]
#34 Executions in Texas and the next largest executing state is
#11 is VA Virginia
###############################################################
#5.F
lm7 <- plm(mrdrte~as.factor(year)+exec+unem,model="fd",data=murder2 %>% subset(state!="TX"))
summary(lm7)
#When Texas is removed from the table has a detterant effect on murder rate in this model
################################################################
#5.G 
lm8 <- plm(mrdrte~as.factor(year)+exec+unem,model="fd", data=murder2)
summary(lm8)
#The deterrant effect has reduced in model lm9 compared to model lm8 
lm9 <- plm(mrdrte~exec+unem,model="pooling",data=murder2 %>% subset(year==90 | year==93))
summary(lm9)
#################################################################
#Question 6.A
wpull <- wpull('AIRFARE')
airfare <- dbReadTable(con,'AIRFARE')
airfare2 <- pdata.frame(airfare,index = c("id","year"))
lm10 <- plm(log(fare)~as.factor(year)+bmktshr+log(dist)+I(log(dist)^2),model="pooling", data=airfare)
summary(lm10)
#0.1 increase in fraction market, biggest carrier will result in 0.3601204 in fare
###################################################################
#6.B
lm11 <- plm(log(fare)~as.factor(year)+bmktshr+log(dist)+I(log(dist)^2),model="fd", data=airfare2)
summary(lm11)
confint(lm10,vcov.=vcovHC) #0.301186070  0.41905465 interval on bmktshr
confint(lm10,vcov.=vcovHC) #0.1759764  0.0284387 interval on bmktshr
#There is a significant change in intervals
###################################################################
#6.C
exp(0.9016003/(2*0.1030196)) #log(dist)/(log(dist)^2)
length(which(airfare$dist == 79.50881))
#4.375868 is the exact point after which the dist becomes positive
#There is no row with this value in the airfare database
##################################################################
#6.D
lm12 <- plm(log(fare)~as.factor(year)+bmktshr+log(dist)+I(log(dist)^2),model='within',data=airfare2)
summary(lm12) #B1 value is now 0.1688590 and it is statisically significant
###################################################################
#6.E
#(i)How famous the brand is (ii)Sufficient Luggage weight allowed
##################################################################
#6.F
#It could be true to a certain level but there are other factors that matter more
###################################################################
#Question 7.A
wpull <- wpull('loanapp')
loanapp <- dbReadTable(con,'loanapp')
lm11 <- glm(approve~white,family="binomial",data=loanapp)
summary(lm11,vcov.=vcovHC)
#approve = 0.8847 + 1.4094(white)The approval rate is 1.4094 more than a Non white apllicant
predict(lm11,loanapp, type="response")
lm12 <- lm(approve~white,data=loanapp)
coeftest(lm12,vcov=vcovHC)
#approve = 0.707792 + 0.200596(white) The approval rate is now reduced to 0.2 more than a 
#non white applicant
#####################################################################
#7.B
lm13 <- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,family="binomial",data=loanapp)
summary(lm13,vcov.=vcovHC) #There is high significance that the approval chances of a white
#applicant is more than that of a non white applicant
#####################################################################
#Question 8.A
wpull <- wpull('alcohol')
alcohol <- dbReadTable(con,'alcohol')
(length(which(alcohol$employ==1))/nrow(alcohol))*100 # % Employed = 89.82  
(length(which(alcohol$abuse==1))/nrow(alcohol))*100  # % Alcohol Abuse = 9.92
####################################################################
#8.B
lm14<-lm(employ~abuse, data=alcohol)
coeftest(lm14) # employ = 0.900995 - 0.028305 abuse 
#The relationship between abuse and employ is as expected and is statistically significant at 
#the 1 % level
####################################################################
#8.C
lm15<-glm(employ~abuse,family="binomial"(link="logit"),data=alcohol)
coeftest(lm15,vcov.=vcovHC) #employ = 2.208325 - 0.283370 abuse 
#The relationship remains the same and it is statistically significant
####################################################################
#8.D
alcohol1 <- (alcohol$abuse==1)
alcohol0 <- (alcohol$abuse==0)
fitted.values(lm14,alcohol1)
fitted.values(lm14,alcohol0)
fitted.values(lm15,alcohol1)
fitted.values(lm15,alcohol0)
max(round(fitted.values(lm14,alcohol1)-fitted.values(lm15,alcohol1),digits=4))
max(round(fitted.values(lm14,alcohol0)-fitted.values(lm15,alcohol0),digits=4))
summary(round(fitted.values(lm14,alcohol1)-fitted.values(lm15,alcohol1),digits=4))
summary(round(fitted.values(lm14,alcohol0)-fitted.values(lm15,alcohol0),digits=4))
#The fitted values from LPM and Logit model gave out the same results both for alcohol abuse and alcohol non abuse.
#Employ rate cannot be more than 100%. Hence, logit regression is not a fitting model here
#######################################################################
#8.E
lm16<-lm(employ~abuse+age+((age)^2)+educ+((educ)^2)+married+famsize+white+northeast+midwest+south+centcity+outercity+qrt1+qrt2+qrt3, data=alcohol)
summary(lm16,vcov.=vcovHC)
#Abuse is statistically significant and has not varied much
#######################################################################
#8.F
lm18<-glm(employ~abuse+age+((age)^2)+educ+((educ)^2)+married+famsize+white+northeast+midwest+south+centcity+outercity+qrt1+qrt2+qrt3, family = "binomial", data=alcohol)
summary(lm18,vcov.=vcovHC) 
#Abuse remains the same as the linear model
#########################################################################
#8.G
LM17<-glm(employ~abuse+age+((age)^2)+exhealth+vghealth+goodhealth+fairhealth+educ+((educ)^2)+married+famsize+white+northeast+midwest+south+centcity+outercity+qrt1+qrt2+qrt3, family = "binomial", data=alcohol)
summary(step(LM17),k=log(nrow(alcohol)))
#The health variables seem very significant in determining the employement
##########################################################################
#8.H
lm18<-glm(employ~abuse+age+((age)^2)+exhealth+fathalc+mothalc+vghealth+goodhealth+fairhealth+educ+((educ)^2)+married+famsize+white+northeast+midwest+south+centcity+outercity+qrt1+qrt2+qrt3, family = "binomial", data=alcohol)
summary(step(lm18),k=log(nrow(alcohol)))
#Now that we have added the mothalc and fathalc to the equation the abuse has become 
#statistically insignificant
###########################################################################
#QUESTION 9.A
wpull <- wpull('fertil1')
fertil1 <- dbReadTable(con,'fertil1')
y74.dummy=rep(0,nrow(fertil1))
y74.dummy[fertil1$year==74]=1
y76.dummy=rep(0,nrow(fertil1))
y76.dummy[fertil1$year==76]=1
y78.dummy=rep(0,nrow(fertil1))
y78.dummy[fertil1$year==78]=1
y80.dummy=rep(0,nrow(fertil1))
y80.dummy[fertil1$year==80]=1
y82.dummy=rep(0,nrow(fertil1))
y82.dummy[fertil1$year==82]=1
y84.dummy=rep(0,nrow(fertil1))
y84.dummy[fertil1$year==84]=1
lm19 <- glm(kids~educ+age+((age)^2)+black+east+northcen+west+farm+othrural+town+smcity+y74.dummy+y76.dummy+y78.dummy+y80.dummy+y82.dummy+y84.dummy, family = "poisson",data=fertil1)
summary(lm19,vcov.=vcovHC)  # y82.dummy = -0.205570 The coefficient on y82 is negative and is significant. The women's fertility rate is 19.3% lower than it was in year 72
##############################################################
#9.B
lm20 <- lm(kids~educ+age+((age)^2)+black+east+northcen+west+farm+othrural+town+smcity+y74.dummy+y76.dummy+y78.dummy+y80.dummy+y82.dummy+y84.dummy,data=fertil1)
summary(lm20,vcov.=vcovHC)
#Black women have 1.051224 more than white women chances of having kids
##############################################################
#9.C
lm21 <- lm(kids~educ+age+(age^2)+black+east+northcen+west+farm+othrural+town+smcity+y74.dummy+y76.dummy+y78.dummy+y80.dummy+y82.dummy+y84.dummy,data=fertil1)
summary(lm21,vcov.=vcovHC)
(cor(fertil1$kids,predict(lm21)))^2 #The R squared in this model is better thatthe poisson fitted model
#############################################################
