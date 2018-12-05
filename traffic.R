#install.packages("smooth")
library(smooth)
library(forecast)

df=read.csv('C:/Users/91944/Documents/R/praxis/Time series/Traffic.csv',stringsAsFactors = FALSE)  #In data frame string treated as factor is set to false
View(df)    #View date
dim(df)     #Dimensions= 747 * 2
ndf=df$X..Vehicles.in.tunnel   #Storing in new dataframe
df$Day=as.Date(df[,1],format='%d-%b-%y')  #information about date convertion format ->  https://www.stat.berkeley.edu/~s133/dates.html
class(df)  #Checking type
df[1,1] #check starting date
k=ts(ndf,start = c(2003,11,01),freq=365) #convert to ts and store
class(k)   #check class   
View(k)    #view data
start(k); end(k); frequency(k)    #Check start,end dates and frequency
plot(k, ylab = "Vehicles in tunnel")   #Plotting data
#Clear view of trend,seasonal effect removed - aggregate
plot(aggregate(k),ylab='Vehicles in tunnel') #Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.
                
par(new=TRUE)   #Combining both plots

#summary of the values for each season can be viewed
#using a boxplot, with the cycle function being used to extract the seasons
#for each item of data. 
boxplot(k ~ cycle(k),axes=FALSE,ylab='')

plot(decompose(k),ylab='Vehicles in tunnel')    #see trend and seasonality
g=diff(k)             #First order diff  for making trend stationary for ARIMA
plot(g)                #plot first order dif
plot(decompose(g))     #check now

#-----------------Moving Averages-----------------------------


abline(reg = lm(k~time(k))) #

boxplot(k~cycle(k))
tra <- ts(k,frequency = 365)
air_decompose <- decompose(air,"multiplicative")
plot(air_decompose)

air_log <- log(tra)
plot.ts(air_log)
head(air_log)


#install.packages("TTR")
library(TTR)

airlog_sma3 <-SMA(air_log,30)
plot.ts(airlog_sma3)
head(airlog_sma3)

airlog_sma3 <-SMA(air_log,60)
plot.ts(airlog_sma3)
head(airlog_sma3)


airlog_sma6 <-SMA(air_log,90)
plot.ts(airlog_sma6)
head(airlog_sma6)


airlog_sma12 <-SMA(air_log,120)
plot.ts(airlog_sma12)


airlog_ema3 <-EMA(air_log,30)
plot.ts(airlog_ema3)
head(airlog_ema3)


airlog_ema3 <-EMA(air_log,60, ratio = 0.3)
plot.ts(airlog_ema3)
head(airlog_ema3)


airlog_ema6 <-EMA(air_log,120)
plot.ts(airlog_ema6)
head(airlog_ema6)


airlog_ema12 <-EMA(air_log,240)
plot.ts(airlog_ema12)
head(airlog_ema12)

uu=auto.arima(g)
plot(g, ylab = "Labour Force Participation Rate 25-54")



holttrend = holt(k, h = 5)
summary(holttrend)
plot(holttrend)



# Phi auto generated
plot(holt(k, h = 15, damped = T))
# To see the generated value for phi
summary(holt(k, h = 15, damped = T))


## phi = 0 means flat curve and 1 means same as original holt trend model. 0.8-0.95 is general reco


# Manual setting of phi
plot(holt(k, h = 15, damped = T, phi = 0.8))




# Overview plot - models
holttrend = holt(k, h = 90)
holtdamped = holt(k, h = 90, damped = T)
arimafore = autoplot(forecast(auto.arima(g), h = 90))

library(ggplot2)
# 3 Forecast Lines as Comparison

s2=sma(k,3,h=80)
n=forecast(summary(naive(k,h=80)))
autoplot(n ,ylab='# Vehicles')
lines(n)
autoplot(forecast(sma(k,3,h=80)))
a1=sqrt(sum((f1$residuals)^2)/744)
summary(s2)

f1=forecast(sma(k,3,h=80))
f1$residuals
s3=sma(k,6,h=80)
f2=forecast(sma(k,6,h=80))
f2$residuals
s4=sma(k,12,h=80)
f3=forecast(s4)
autoplot(f1)
a2=sqrt(sum((f2$residuals)^2)/741) 
a3=sqrt(sum((s4$residuals)^2)/735)
autoplot(f2)
autoplot(f3)
a1
a2
a3

library(TTR)
sh2=EMA(k,3,h=80)
d=forecast(sh2)
d$residuals
ah1=sqrt(sum((d$residuals)^2)/744)

sh3=EMA(k,6,h=80)
sh4=EMA(k,12,h=80)
d2=forecast(sh3)
d2$residuals
ah2=sqrt(sum((d2$residuals)^2)/741) 
d3=forecast(sh4)
d3$residuals
ah3=sqrt(sum((d3$residuals)^2)/735)
ah1
ah2
ah3
autoplot(d)
autoplot(d2)
autoplot(d3)
forecast::autolayer(sh2$mean, series ="SMA" )




holttrend = holt(k, h = 90)
holtdamped = holt(k, h = 90, damped = T)
#arimafore = forecast(au9o.arima(g), h = 90)

library(ggplot2)
# 3 Forecast Lines as Comparison
autoplot(k) +
  forecast::autolayer(holttrend$mean, series = "Holt Linear Trend") +
  forecast::autolayer(holtdamped$mean, series = "Holt Damped Trend") +
  xlab("year") + ylab("# Vehicles") + 
  guides(colour=guide_legend(title="Forecast Method")) + theme(legend.position = c(0.8, 0.2)) +
  ggtitle("Forecast HoltsWinters") + theme(plot.title=element_text(family="Times", hjust = 0.5, color = "black",
                                                      face="bold", size=15))

