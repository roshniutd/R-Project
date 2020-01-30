#QUESTION 6
bitcoinV2 <- fread('PS3Problem6.csv')
bitcoinV2$date <- as.Date(bitcoinV2$date)
bitcoinV2$date
ts.plot(bitcoinV2$bitcoin, gpars = list(xlab = "day" , ylab = "bitcoin"))
################################################################
lm <- lm(bitcoin~SP500+gold+oil+euro,data=bitcoinV2)
summary(lm)
################################################################
rep.kpss <- function(series,alpha=0.05,dmax=5){
  diff <- 0
  for(i in 1:dmax){
    suppressWarnings(pval <- kpss.test(series,null="Level")$p.value)
    if(pval>=alpha){
      return(c(diff,0,pval))
    }
    suppressWarnings(pval <- kpss.test(series,null="Trend")$p.value)
    if(pval>=alpha){
      return(c(diff,1,pval))
    }
    diff <- diff + 1
    series <- diff(series)
  }
  return(NULL)
}
rep.kpss(bitcoinV2$bitcoin)
rep.kpss(bitcoinV2$SP500)
rep.kpss(bitcoinV2$gold)
rep.kpss(bitcoinV2$oil)
rep.kpss(bitcoinV2$euro)
#################################################################
n <- nrow(bitcoinV2)
lm <- lm(diff(bitcoin)~diff(SP500)+diff(gold)+diff(oil)+diff(euro)+as.numeric(date)[2:n],data=bitcoinV2)
summary(lm)
#We see that none of these variables have an effect on bitcoin
#################################################################
bitcoinV2 <- bitcoinV2[date>=as.Date('2017-01-01')]
ggplot(bitcoinV2,aes(x=date,y=bitcoin)) + geom_line() 
ggplot(bitcoinV2,aes(x=date,y=SP500)) + geom_line()
ggplot(bitcoinV2,aes(x=date,y=gold)) + geom_line()
ggplot(bitcoinV2,aes(x=date,y=oil)) + geom_line()
ggplot(bitcoinV2,aes(x=date,y=euro)) + geom_line()
#################################################################
acf(diff(bitcoinV2$bitcoin))
acf(diff(bitcoinV2$SP500))
acf(diff(bitcoinV2$gold))
acf(diff(bitcoinV2$oil))
acf(diff(bitcoinV2$euro))

pacf(diff(bitcoinV2$bitcoin))
pacf(diff(bitcoinV2$SP500))
pacf(diff(bitcoinV2$gold))
pacf(diff(bitcoinV2$oil))
pacf(diff(bitcoinV2$euro))
##################################################################

outp1 <- matrix(0L,7^2,3)
row <- 1
for(p in 0:6){
  for(q in 0:6){
    aic <- AIC(arima(log(bitcoinV2$bitcoin),c(p,1,q)))
    outp1[row,] <- c(p,q,aic)
    row <- row + 1
  }
}
order(outp1[,3])
outp1[38,]
#################################################################
lm <- stats::arima(log(bitcoinV2$bitcoin),c(5,1,2),seasonal=list(order=c(1,0,1),period=12))
steps <- 30
future <- forecast(lm,h=steps)
plot(future)
##################################################################
periodogram(bitcoinV2$bitcoin)
#Becouse the data is not stationary
periodogram(diff(bitcoinV2$bitcoin))
#################################################################
diff_jp <- function(x){
  n <- nrow(x)
  return(x[2:n,]-x[1:n-1,])
}
x <- bitcoinV2 %>% dplyr::select(bitcoin,SP500,gold,oil,euro) %>% diff_jp
VAR(x,p=1,type="both") %>% AIC
VAR(x,p=2,type="both") %>% AIC
VAR(x,p=3,type="both") %>% AIC
VAR(x,p=4,type="both") %>% AIC
VAR(x,p=5,type="both") %>% AIC

lm <- VAR(x,p=2,type="both")
library(broom)
summary(lm)
######################################################################
causality(lm,cause='SP500')$Granger
causality(lm,cause='gold')$Granger
causality(lm,cause='oil')$Granger
causality(lm,cause='user')$Granger
###################################################################
bitcoinV2$bitcoin[n]+cumsum(predict(lm,n.ahead=30)$fcst$bitcoin[,1])
######################################################################

