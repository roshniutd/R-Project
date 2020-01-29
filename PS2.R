library(DBI) #coverts r to 
library(RSQLite) 
library(data.table)
library(broom)
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
#################Question 1#############################################
vote1 <- dbReadTable(con,'vote1')
Bexpend <- log(vote1$expendB)
Aexpend <- log(vote1$expendA)
lm <- lm(formula=voteA~Aexpend+Bexpend+prtystrA,data =vote1)
summary(lm) #voteA=45.08788+6.08136(Aexpend)-6.61563(Bexpend)+0.15201(prtystrA)
#For 1000$ increase in expenditure there is a increase of 6.06% increase in Vote A
##########################################################################
#H0: expend A= - expend B or #B1 = -B2
##########################################################################
#voteA = 45 + 6.08log(expendA) - 6.62log(expendB) + 0.15201(prtystrA)
#A's expenditure affects the model(increases Votes) by 0.608
#B's expenditure also affects the model(decreases votes) by 0.662
#No these results are not sufficient to test the hypothesis in part 2
##########################################################################
Hypothesis1 <- log(vote1$expendB/vote1$expendA)
lm <- lm(formula=voteA~(log(expendA))+Hypothesis1+prtystrA,data =vote1)
summary(lm)
#voteA = 45.08788-0.53427(log(expendA))-6.61563log((expendB/expendA))+0.15201(prtystrA)
#Theta= -0.53427 #Theta error = 0.53311
T_Test <- -0.53427 / 0.53311 # |-1.002| #1.002<1.960  
###########################################################################
#Since T_Test value is rejected in 5% and P Value is also less than 0 
#Reject H0, a 1% increase in A's expenditures is not offset by a 
#1% increase in B's expenditures
#############################Question 2####################################
LAWSCH85 <- dbReadTable(con,'LAWSCH85')
LAWSCH85
lm <- lm(log(salary)~LSAT+GPA+(log(libvol))+(log(cost))+(rank),data=LAWSCH85)
summary(lm) #8.34+0.0047(LSAT)+0.247(GPA)+0.095(log(libvol))+0.0375(log(cost))-0.0033(rank)
#H0 = B5=0 #t stat = -0.0033246/0.0003485 = -9.541
#|-9.541| > 1.960 (T Test table value for 95% Confidence)
#P value is also lesser than 0 
#H0 is rejected 
##########################################################################
lm21 <- lm(log(salary)~LSAT+GPA,data=LAWSCH85)
summary(lm21) # R^2 = 0.6282
T_LSAT <- 0.026726/0.004871 #4.7260 insignificant in the 95 %
T_GPA <-  0.528941/0.113364 #4.2089 Significant in the 95%
lm22 <- lm(log(salary)~I(LSAT+GPA),data=LAWSCH85)
summary(lm22) # R^2 = 0.5791
T_LSAT_GPA <- 0.043346/0.003123 #13.877 it is insignificant in 5% level
# The features are individually more significant 
#######################################################################
lm23 <- lm(log(salary)~LSAT+GPA+(log(libvol))+(log(cost))+(rank)+I(clsize+faculty),data=LAWSCH85)
summary(lm23)
#R-squared:  0.844. No the model contributes to 84.4 % increase in salary 
#while the original model had R-squared: 0.8417 . The increase is quiet low(0.03)
#with the to added variables
########################################################################
LAWSCH85 <- wpull('LAWSCH85')
#libvol,faculty,cost,north,south,east and west influence the rank of the law school
################Question 3#############################################
hprice1 <- dbReadTable(con,'hprice1')
hprice1
lm <- lm(log(price)~sqrft +bdrms,data=hprice1)
summary(lm) 
beta1 = I(hprice1$sqrft-(150*hprice1$bdrms))
lm3 <- lm((log(price))~beta1+bdrms,data=hprice1)
summary(lm3)
coefficients(lm3)
#Theta1 = 0.085801367 Theta1 error = 0.002677
##########################################################################
#B2 <- Theta1 - (150*(B1))
B2 <- 0.0857945 - (150*(0.0003794))
lm <- lm(log(price)~sqrft +I(0.0288845*(bdrms)),data=hprice1)
summary(lm) #log(price)=4.766027213+0.000379446sqrft+0.999998863(0.0288845 * (bdrms))
########################################################################
beta1 = I(hprice1$sqrft-(150*hprice1$bdrms))
lm3_2 <- lm((log(price))~beta1+bdrms,data=hprice1)
summary(lm3_2) #t value = 3.205 p value is 0
tidy(lm3_2)
tidy(lm3_2,conf.int = T,conf.level = 0.95)
#The confidence interval is 0.0326 and 0.139
#####################Question 4#########################################
#H0:b2+1 = b3+1 or b2=b3 #Theta=b3+1-b2-1 #b3-b2 
wage2 <- dbReadTable(con,'wage2') 
#Complicated hypothesis testing
lmC <- lm(log(wage)~ educ + I(exper + tenure)+tenure,data = wage2)
summary(lmC) #l.wage=5.496696+0.074864(educ)+0.015328(exper+tenure)-0.001954tenure

#######################################################################
T_Stat <- -0.001954/0.004743  #|-0.412|=0.412  
tidy(lmC,conf.int=TRUE,conf.level = 0.05)
#0.412 < 1.960 We reject null in 5% #P value is 0.681/;"

#####################Question 5########################################
subs401k <- dbReadTable(con,'401ksubs') 
length(which(subs401k$fsize==1)) #There are 2017 families with a family size of 1
########################################################################
subs401k <- dbReadTable(con,'401ksubs') 
subs_fsize<-subset(subs401k, fsize==1)
lm5_1 <- lm(nettfa~inc+age,data=subs_fsize) #or
lm5_1 <- lm(nettfa~inc+age,data=subs401k,fsize==1)
summary(lm5_1) # nettfa=-43.03981+0.79932(inc)+0.84266(age)
#The intercept is a negative value and so it seems surprising. The other 
#coefficients make sense and so there are no surprises there
########################################################################
#The intercept (-43.03981) looks fairly low . In the given model  
#the intercept will pull down the nettfa by a huge factor
########################################################################
lm5_1 <- lm(nettfa~inc+age,data=subs_fsize)
summary(lm5_1) # nettfa=-43.03981+0.79932(inc)+0.84266(age)
#H0 : B2 = 1 
T_value = (0.84266-1)/0.09202 #|-1.7098 | = 1.7098 < 2.576
#P Value is close to zero <2e-16
#We reject the null at 1 % significance level
#######################################################################
lm5_2 <- lm(nettfa~inc,data=subs_fsize)
summary(lm5_2) #nettfa=-10.5709 + 0.8207(inc)
Diff <- 0.8207 - 0.79932 #The difference is 0.02 and is not very significant
#this could be due to the fact that this model does not have age
#####################Question 6#######################################
kielmc <- dbReadTable(con,'kielmc')
kielmc <- wpull('kielmc') 
subset2<-subset(kielmc, year==1981)
lm6_1 <- lm((log(price))~(log(dist)),data = subset2)
summary(lm6_1) # (log(price)) = 8.04716+0.36488(log(dist))
#The price increases by 0.36 for every increase in distance
#I would have expected the B1 value to be high 
######################################################################
lm6_2<-lm((log(price))~(log(dist))+(log(intst))+(log(land))+rooms+ 
            baths+age,data = subset2)
summary(lm6_2)
anova(lm6_1,lm6_2)
#price=9.584027+0.038105log(dist)-0.037579log(intst)+0.089009
               #log(land)+0.077623(rooms)+0.266547(baths)-0.002500(age)
#The price increases by 0.038 only for every increase in distance 
#We see that there are other variables that affect the increase in price of the house
#even more than distance
######################################################################
lm6_3<-lm((log(price))~(log(dist))+(log(intst))+(log(land))+rooms+ 
            baths+age+I((log(intst))^2),data = kielmc,year==1981)
summary(lm6_3)#0.545477+0.146375(log(dist))+1.753239(log(intst))
#   +0.102652(log(land))+0.077607rooms+0.262400baths-0.001844age
#   -0.101174(I((log(intst))^2) 
#coefficient on log(intst^2) is -0.101174 and  its t-stat=3.341,
# so it is  so significant
#######################################################################
lm6_4<-lm((log(price))~(log(dist))+(log(intst))+(log(land))+rooms+ 
  baths+age+I((log(intst))^2)+I((log(dist))^2),data = kielmc,year==1981)
summary(lm6_4) # t stat = 1.053 and p value is 0.29 so it is insignificant 
#####################Question 7#######################################
wage1 <- dbReadTable(con,'wage1')
lm7_1 <- lm(log(wage) ~ educ + exper +I(exper^2), data=wage1)
summary(lm7_1)
#log(wage)=0.13+0.09(educ)+0.04(exper)-0.0007(exper^2)
######################################################################
T_Stat <- ((-0.0007121)/0.0001160) #|-6.141| = 6.141 > 2.576
#p value is 1.63e-09 
#Thus we do not reject the null and it is statistically significant at 1%
######################################################################
#Change in wage with respect to exper = B2+(2*B3*exper)
#exper=5 #B2=0.04 #B3=-0.0007
Exper_5 <- 0.04 + (2*(-0.0007)*5) #0.033
Exper_20 <- 0.04 + (2*(-0.0007)*20) #0.012
######################################################################
#In the sample, there are 121 people with at least 29 years of experience.
experexp = 0.041/(2*(.000714)) #28.7 years of experience
length(which(wage1$exper>=29))
#####################Question 8#######################################
wage1 <- log(wage2$wage)
lm8_1 <- lm(wage1~educ+exper+I(educ*exper),data = wage2)
summary(lm8_1) #Making exper constant
#d wage1 = b1.d educ+b3.d educ*exper
#d wage1 = (b1+b3exper) d educ
#d wage1 /d educ = (b1+b3exper)
#Thus the above equation is the return to another year of education (in decimal form)
# holding experience fixed
######################################################################
#H0:b3 = 0 
#H1:b3 > 0 
#b3 coefficient returns the relation between both educ and exper
######################################################################
lm8_2 <- lm(wage1~educ+exper+I(educ*exper),data = wage2)
summary(lm8_2) #wage=5.949455+0.044050(educ)-0.021496(exper)+(0.003203)I(educ*exper)
#H0:b3 = 0 
T_value <- (0.003203-0)/0.001529 # 2.095 > 1.960 
#We reject the null value in 5% significance 
######################################################################
#Substituting Theata value in the above model we get
#log(wage)=b0+Theta(educ)+b3(exper-10)+b2
Beta3 <- (wage2$educ*(wage2$exper-10))
lm8_3 <- lm(wage1~educ+exper+Beta3,data = wage2)
summary(lm8_3)
tidy(lm8_3,conf.int = 0.95)
#Theata1 value = 0.0761 Error = 0.00662
#The confidance interval is 0.0631 and 0.0891
#####################Question 9#######################################
gpa2 <- wpull('gpa2')
gpa2 <- dbReadTable(con,'gpa2')
lm9_1 <- lm(sat~hsize+I((hsize)^2),data=gpa2)
summary(lm9_1) #sat=997.981+19.814(hsize)-2.131(hsize^2)
#For each increase in combined SAT score the quadratic term signifies a decrease by 
#213 of the graduating class so it is statistically significant
#######################################################################
#hsize* = 19.814/2(2.13) = 4.65(in hundreds) 465 students is optimal
1540 =997.981+(19.814*hsize)-(2.131*(hsize^2))
#######################################################################
#No the analysis is not representative of all the high school students since 
#the high school percentile is only 43.47% and we dont know if all the students are
#accounted for
#######################################################################
sat1 <- log(gpa2$sat)
lm9_2 <- lm(sat1~hsize+I((hsize)^2),data=gpa2)
summary(lm9_2) #sat=6.8960291+0.0196029(hsize)-0.0020872(hsize^2)
#hsize* = 0.0196029/(2*0.0020872)= 4.69 in 100's
#It is not different from the previous model
#####################Question 10########################################
hprice1 <- dbReadTable(con,'hprice1')
hprice1
lm1 <- lm(log(price)~log(lotsize) +log(sqrft)+bdrms,data=hprice1)
#Multiple R-squared:  0.643
summary(lm1) # -1.29704+0.16797(log(lotsize))+0.70023(log(sqrft))+0.03696(bdrms)
#########################################################################
Price <- (20000*(0.16797)) +(2500*(0.70023))+(4*0.03696)
Price # Predicted price = 5110.123
#########################################################################
lm <- lm(price~lotsize+sqrft+bdrms,data=hprice1)
summary(lm) #Multiple R-squared:  0.6724
#lm1 <- lm(log(price)~log(lotsize) +log(sqrft)+bdrms,data=hprice1)
#This model seems better based on R value 
########################################################################











