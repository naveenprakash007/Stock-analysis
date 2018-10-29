library('ggplot2')
library('forecast')
library('tseries')
library(xts)
tcs= read.csv('/cloud/project/TCS.csv', header=TRUE, stringsAsFactors=FALSE)
class(tcs)
str(tcs)
tcs$Date1 = as.Date(tcs$Date)


#cleaning data
Adjclose_ts = ts(tcs[, c('Adj.Close')])
tcs$cleanclose = tsclean(Adjclose_ts)


#taking care of holidays
X <- as.xts(tcs[,-1], order.by = tcs$Date1)
a<-na.locf(merge(X, seq(min(tcs$Date1), max(tcs$Date1), by = 1)))
b<-data.frame(date=index(a), coredata(a))

#moving average
b$mv4 = ma(b$cleanclose, order=28)
b$mv16 = ma(b$cleanclose, order=112)
b$mv32 = ma(b$cleanclose, order=224)
b$mv52 = ma(b$cleanclose, order=364)




#Window
win10<-rollapply(b$cleanclose,10,mean)
win75<-rollapply(b$cleanclose,75,mean)
plot(win10)
plot(win75)





l1<-diff(b$Volume)
k1<-diff(b$Adj.Close)

#volume shocks
df <- (AAA = rep(c(0,l1)))
b$difvol<-df
b$volprof<-ifelse(b$difvol>(b$Adj.Close/50),1,0)
volumeee= ts(b[, c('volprof')])

#Price shocks 
df <- (AAA = rep(c(0,k1)))
b$difpri<-df
b$priceprof<-ifelse(b$difpri>(b$Adj.Close/50),1,0)
pricee=ts(b[,c('priceprof')])


#Pricing shock without volume shock - based on points a & b 
b$volpri<-ifelse(b$priceprof==1 & b$volprof==0,1,0)
onlyprice=ts(b[,c("volpri")])








#Create timeseries plot of close prices
ggplot(b, aes(date, cleanclose)) + geom_line()  + scale_x_date('date') + ylab("Daily price") +
  xlab("")



#Color timeseries in simple blue color.
ggplot() +
geom_line(data = b, aes(x = date, y = cleanclose, colour = "Close")) +
  ylab('overall price')



#Color timeseries between two volume shocks
ggplot() +
  geom_line(data = b, aes(x = date, y = difvol, colour = "Close")) +
  ylab('overall price')
b$mv52 = ma(b$cleanclose, order=364)  


#Gradient color in blue spectrum based on difference of 52 week moving average
ggplot()+
  geom_line(data = b, aes(x = date, y = mv52,   colour = "yearly Moving Average"))  +
  geom_path(stat = "identity",
            position = "identity")+
  ylab('overall price')


#Mark closing Pricing shock without volume shock
ggplot() +
  geom_line(data = b, aes(x = date, y = volpri, colour = "Close")) +
  ylab('vol price')

#Hand craft partial autocorrelation plot
Pacf(onlyprice,main='')
