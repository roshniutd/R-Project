library(data.table)
library(dplyr)
library(partykit)
library(plm)
library(ggplot2)


#1
n <- 500
set.seed(75080)

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 50
y   <- -100*z+ 1100 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
data1 <- data.table('id'=1:500,'sat'=y,'income'=x,'group'=rep(1,n))

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 80
y   <- -80*z+ 1200 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)

y   <- ifelse(y>1600,1600,y)
data2 <- data.table('id'=501:1000,'sat'=y,'income'=x,'group'=rep(2,n))

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 30
y   <- -120*z+ 1000 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
data3 <- data.table('id'=1001:1500,'sat'=y,'income'=x,'group'=rep(3,n))

dtable <- merge(data1    ,data2, all=TRUE)
dtable <- merge(dtable ,data3, all=TRUE)

#We can observe that thare are three set of groups and The data generating process is done using three groups. There are  lower,middle,higher income groups , Within every group,SAT score is negatively related to income level. 
#but in Between groups, the effect might  be different, which is  not  obvious from  data generating process.

#############################################

#2
dtable$group <- as.factor(dtable$group)
dtable$iid <- 1:1500

pooled <- lm(sat~income,data=dtable)
within <- lm(sat~income+group-1,data=dtable)
modelv1 <- lm(sat~income,data=dtable[group==1])
modelv2 <- lm(sat~income,data=dtable[group==2])
modelv3 <- lm(sat~income,data=dtable[group==3])

summary(pooled)

summary(within)
summary(modelv1)
summary(modelv2)
summary(modelv3)
# As observed the  effect of income between groups on SAT is explained by the pooled model. Although it is evident that income and SAT are negatively correlated within-groups,we find that income and SAT are positively correlated in total.

#############################################

#3

ctree(sat~income+group,data=dtable)
ctree(sat~income,data=dtable)
ctree(sat~group,data=dtable)
plot(ctree(sat~group,data=dtable))
# As we can see that splitting based on income makes really does not add much of a value.  The income variable have a continuous relationship with SAT ,so the trees are very different and uncontrollable here. 
# It is better that tree here is based on the group variable.

#############################################

#4

glmtree(sat~income|group,data=dtable)
plot(glmtree(sat~income|group,data=dtable))

# After running the model we were able to draw the conclusion based on comprarission to previous ones that glmtree is perfect model as this is the data generating process, linear models based on splits defined by the three groups.
# splitting on the group variable and including income in linear modeling process is the most important to be considered.

#############################################

#5

kmeansplot <- function(wss){
  wss <- data.table('Centers'=1:NROW(wss),'wss'=wss)
  ggplot(wss,aes(x=Centers,y=wss)) + geom_line() +
    scale_y_continuous('Within group sum of squares') +
    scale_x_discrete(limits=1:10) + 
    ggtitle('k-Means Elbow Plot') +
    theme(aspect.ratio=1,plot.margin=grid::unit(c(0,0,0,0), "mm"))
}

wss <- rep(NA,10)
for(i in 1:10)
  wss[i] <- kmeans(dtable%>%select(sat,income),i,nstart=10)$tot.withinss
kmeans(dtable%>%select(sat,income),2)$centers
kmeans(dtable%>%select(sat,income),3)$centers

#Thus the optimal number of groups are 3

#############################################

#6
dtable$kmean_31 <- as.factor(kmeans(dtable%>%select(sat,income),3)$cluster)
table(dtable$kmean_31,dtable$group)

dtable$kmean_21 <- as.factor(kmeans(dtable%>%select(sat,income),2,nstart=10)$cluster)
table(dtable$kmean_21,dtable$group)

dtable$hclust <- as.factor(cutree(hclust(dtable%>%select(sat,income)%>%dist),3))
table(dtable$hclust,dtable$group)
# identification rate for kmeans (from the matrix table) : (256+343+267)/1500=57.8%
#identification rate for kmeans (from the matrix table) : (401+202+134)/1500=49.13%
#Using the kmeans, It is often almost impossible to identify the number of clusters for the data. 
# The hierarchical cluster too doesn't do well and its not better than k-cluster. 

#############################################
#7

summary(pooled <- lm(sat~income,data=dtable))
summary(model123 <- lm(sat~income+kmean_31-1,data=dtable))
summary(model223 <- lm(sat~income+kmean_21-1,data=dtable))
summary(model323 <- lm(sat~income+hclust-1,data=dtable))

#The negative relationship between income and SAT score is not reflected in these models. So we will not be able to find the relationship which was there from the data generating process.
# My prespective is that because of the fact that we did not scale the data

#############################################
#8
wss <- rep(NA,10)
for(i in 1:10)
  wss[i] <- kmeans(dtable%>%select(income),i,nstart=10)$tot.withinss
kmeansplot(wss)


kmeans(dtable%>%select(income),2)$centers
dtable$kmean_21 <- as.factor(kmeans(dtable%>%select(income),2)$cluster)
table(dtable$kmean_21,dtable$group)
summary(table(dtable$kmean_21,dtable$group) )

kmeans(dtable%>%select(income),3)$centers
dtable$kmean_31 <- as.factor(kmeans(dtable%>%select(income),3)$cluster)
table(dtable$kmean_31,dtable$group)


summary(modelh123 <- lm(sat~income+kmean_21-1,data=dtable))

summary(modelh223 <- lm(sat~income+kmean_31-1,data=dtable))

#The K3 means reflects the negative relationship between and SAT score. 
# identification rate for kmeans (from the matrix table) we find : (489+500+484)/1500=98.2% Which is very good. 
# As we can observe the estimation accuracy is 98.2%

#############################################
#9
wss <- rep(NA,10)
for(i in 1:10)
  wss[i] <- kmeans(dtable%>%select(sat,income)%>%scale,i,nstart=10)$tot.withinss
kmeansplot(wss)

kmeans(dtable%>%select(income,sat)%>%scale,3)$centers
dtable$kmean_311 <- as.factor(kmeans(dtable%>%select(income,sat)%>%scale,3,nstart=10)$cluster)
table(dtable$kmean_311,dtable$group)

summary(modeli1 <- lm(sat~income+kmean_311-1,data=dtable))

#As we can see that after including the scaling, we can find the negative relationship. 
#Caluclating identification rate for kmeans (from the matrix table) : (371+500+330)/1500=80% W
#The estimation accuracy is 80% which is not as good as what we did when running the kmeans estimation using only the income variable. 







