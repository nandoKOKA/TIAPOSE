library(forecast)
library(rminer)

# example with R data AirPassengers
# other time series could be read from a CSV file via read.table
data(AirPassengers)

H=12 # number of ahead predictions
L=length(AirPassengers)
# time series monthly object:
TR=ts(AirPassengers,frequency=12,start=1,end=L-H)

pdf("air.pdf")
tsdisplay(TR)
dev.off()

# holt winters method
HW=HoltWinters(TR)
F=forecast(HW,h=H) # 1 to H ahead forecasts
Pred=F$mean[1:H] # HW predictions
Target=AirPassengers[(L-H+1):L]
txt=paste("HW SMAPE:",round(mmetric(Target,Pred,metric="SMAPE"),2),"\n")
mgraph(Target,Pred,graph="REG",Grid=10,col=c("black","blue"),
       leg=list(pos="topleft",leg=c("target","predictions")),main=txt,PDF="hw")

# arima method:
AR=auto.arima(TR)
F1=forecast(AR,h=H) # 1 to H ahead forecasts
Pred1=F1$mean[1:H] # AR predictions
txt=paste("AR SMAPE:",round(mmetric(Target,Pred1,metric="SMAPE"),2),"\n")
mgraph(Target,Pred1,graph="REG",Grid=10,col=c("black","blue"),
       leg=list(pos="topleft",leg=c("target","predictions")),main=txt,PDF="ar")

# neural network modeling:
d=CasesSeries(AirPassengers,c(1,12,13)) # data.frame from time series
LD=nrow(d)
dtr=1:(LD-H) # train indices
NN=fit(y~.,d[dtr,],model="mlpe")
# from 1 to H ahead forecasts:
Pred2=lforecast(NN,d,start=(LD-H+1),horizon=H)
txt=paste("NN SMAPE:",round(mmetric(Target,Pred2,metric="SMAPE"),2),"\n")
mgraph(Target,Pred2,graph="REG",Grid=10,col=c("black","blue"),
       leg=list(pos="topleft",leg=c("target","predictions")),main=txt,PDF="nn")
