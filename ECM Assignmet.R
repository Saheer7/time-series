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
plot(aggregate(k)) #Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.

par(new=TRUE)   #Combining both plots

#summary of the values for each season can be viewed
#using a boxplot, with the cycle function being used to extract the seasons
#for each item of data. 
boxplot(k ~ cycle(k),axes=FALSE,ylab='')

plot(decompose(k))    #see trend and seasonality
g=diff(k)             #First order diff  for making trend stationary for ARIMA
plot(g)                #plot first order dif
plot(decompose(g))     #check now





library(TTR)

airlog_sma3 <-SMA(g,3)
airlog_sma3
plot.ts(airlog_sma3)
head(airlog_sma3)
plot(forecast(airlog_sma3))


airlog_sma6 <-SMA(g,6)
plot.ts(airlog_sma6)
head(airlog_sma6)


airlog_sma6 <-SMA(g,12)
plot.ts(airlog_sma6)
head(airlog_sma6)


airlog_ema3 <-EMA(g,3)
plot.ts(airlog_ema3)
head(airlog_ema3)

airlog_ema3 <-EMA(g,6)
plot.ts(airlog_ema3)
head(airlog_ema3)
plot(forecast(airlog_ema3))

seasonplot(g,
           col = c("red","blue","green"),
           bty ="l",
           type = "l",
           year.labels = T,main = 'Seasonal plot')
legend("topleft", legend=c("2003", "2004","2005"),
       col=c("red", "blue","green"), lty=1:2, cex=0.4,box.lty=2)



#par(mfrow=c(1,1))
plot(g)
tsdisplay(g)
library(tseries)

adf.test(g) # Stationarity   #since our p-value is 0.01 we will reject the null hypo of non stationary...our data is stationary

tsdisplay(g) # Autocorrelation
## pacf :  Degree of association between two variables while adjusting for effect 
## of one or more additional variable
## acf  :  Similarity between values of the same variable across observations 
## pacf is used for p (auto regressive component which is regression of value with lag time) and 
## acf is used for ## q (moving average component which is regression of error with its lag)


## If acf has less bars outside the boundary compared to pacf, then, 
## we will use acf which is q and p =0 

## If pacf has less bars outside the boundary compared to acf, then, 
## we will use pacf which is p and q =0




mode<-auto.arima(g)
refit<-Arima(g,model=mode)
accuracy(refit)


# Upper case P,D,Q is for seasonal component of ARIMA model
# Parameters of ARIMA model

auto.arima(g, trace = T)   #(4,0,3)  is the best values for arima   (ar is pacf,,,,ma is acf)

#---------------Holts Winter

# Phi auto generated
plot(holt(k, h = 90, damped = T))
# To see the generated value for phi
summary(holt(k, h = 90, damped = T))

# Overview plot - models
holttrend = holt(g, h = 90)
holtdamped = holt(g, h = 90, damped = T)
arimafore = forecast(auto.arima(g), h = 90)
accuracy(holttrend)
accuracy(holtdamped)

library(ggplot2)
# 3 Forecast Lines as Comparison
autoplot(g) +
  forecast::autolayer(holttrend$mean, series = "Holt Linear Trend") +
  forecast::autolayer(holtdamped$mean, series = "Holt Damped Trend") +
  forecast::autolayer(arimafore$mean, series = "ARIMA") +
  xlab("year") + ylab("Vechicles") + 
  guides(colour=guide_legend(title="Forecast Method")) + theme(legend.position = c(0.8, 0.2)) +
  ggtitle("Traffic") + theme(plot.title=element_text(family="Times", hjust = 0.5, color = "blue",
                                                      face="bold", size=15))


#Naive

naive()