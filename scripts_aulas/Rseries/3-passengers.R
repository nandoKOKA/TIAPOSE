# 3-passengers.R: univariate multi-step ahead time series forecasting example: passengers.ts.
# this series contains a 12 month seasonal pattern and a growing trend.
# in the past, it was well studied by conventional time series forecasting methods (e.g., ARIMA).
#
# in this demo, there are a total of 12 forecasts: 1-ahead, 2-ahead, ..., 12-ahead (thus multi-step ahead forecasting)

# setwd("") # adjust working directory if needed.

# install.packages("forecast")
library(forecast)
library(rminer)

cat("read passenger time series:")
TS=read.table("passengers.ts",header=TRUE,sep=";")
TS=TS[,1] # vector of numeric

K=12 # TS period (monthly!)
print("show graph")
tsdisplay(TS)
mpause()

L=length(TS)
NTS=K # number of predictions
H=NTS # from 1 to H ahead predictions

# --- this portion of code uses forecast library, which assumes several functions, such as forecast(), and uses a ts object 
# --- note: the forecast library works differently than rminer
# time series monthly object, frequency=K 
# this time series object only includes TRAIN (older) data:
LTR=L-H
# start means: year of 1949, 1st month (since frequency=K=12).
# according to the ts function documentation: frequency=7 assumes daily data, frequency=4 or 12 assumes quarterly and monthly data
TR=ts(TS[1:LTR],frequency=K,start=c(1949,1)) # start means: year of 1949, 1st month (since frequency=K=12).
# show the in-sample (training data) time series:
plot(TR)
print(TR)
mpause() # press enter

# target predictions:
Y=TS[(LTR+1):L]

# holt winters forecasting method:
print("model> HoltWinters")
HW=HoltWinters(TR)
print(HW)
plot(HW)
print("show holt winters forecasts:")
# forecasts, from 1 to H ahead:
F=forecast(HW,h=H)
print(F)
Pred=F$mean[1:H] # HolWinters format
mgraph(Y,Pred,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","HW pred.")))
cat("MAE:",mmetric(Y,Pred,metric="MAE"),"\n")
mpause() # press enter

# arima modeling:
print("model> auto.arima")
AR=auto.arima(TR)
print(AR) # ARIMA(3,0,1)(2,1,0)[12] 
print("show ARIMA forecasts:")
# forecasts, from 1 to H ahead:
F1=forecast(AR,h=H)
print(F1)
Pred1=F1$mean[1:H]
mgraph(Y,Pred1,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target"," ARIMA pred.")))
cat("MAE:",mmetric(Y,Pred1,metric="MAE"),"\n")
mpause() # press enter

# NN from forecast:
print("model> nnetar")
NN1=nnetar(TR,P=1,repeats=3)
print(NN1)
F3=forecast(NN1,h=H)
Pred3=F3$mean[1:H] # HolWinters format
mgraph(Y,Pred3,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","NN1 pred.")))
cat("MAE:",mmetric(Y,Pred3,metric="MAE"),"\n")
mpause() # press enter

# ets from forecast:
print("model> ets")
ETS=ets(TR)
F4=forecast(ETS,h=H)
Pred4=F4$mean[1:H] # HolWinters format
mgraph(Y,Pred4,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","ets pred.")))
cat("MAE:",mmetric(Y,Pred4,metric="MAE"),"\n")
mpause() # press enter

# -- end of forecast library methods

# neural network modeling, via rminer:
print("model> mlpe (with t-1,t-12,t-13 lags)")
d=CasesSeries(TS,c(1,12,13)) # data.frame from time series (domain knowledge for the 1,12,13 time lag selection)
print(summary(d))
LD=nrow(d) # note: LD < L
hd=holdout(d$y,ratio=NTS,mode="order")
NN2=fit(y~.,d[hd$tr,],model="mlpe")
# multi-step, from 1 to H ahead forecasts:
init=hd$ts[1] # or same as: init=LD-H+1
# for multi-step ahead prediction, the lforecast from rminer should be used instead of predict,
# since predict only performs 1-ahead predictions
F5=lforecast(NN2,d,start=hd$ts[1],horizon=H)
print(F5)
Pred5=F5
mgraph(Y,Pred5,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","NN2 pred.")))
cat("MAE:",mmetric(Y,Pred5,metric="MAE"),"\n")
mpause() # press enter

# linear regression modeling ("lm"), via rminer:
print("model> lm (with t-1,t-12,t-13 lags)")
LM=fit(y~.,d[hd$tr,],model="lm")
# multi-step, from 1 to H ahead forecasts:
init=hd$ts[1] # or same as: init=LD-H+1
# for multi-step ahead prediction, the lforecast from rminer should be used instead of predict,
# since predict only performs 1-ahead predictions
F6=lforecast(LM,d,start=hd$ts[1],horizon=H)
print(F6)
Pred6=F6
mgraph(Y,Pred6,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","LM pred.")))
cat("MAE:",mmetric(Y,Pred5,metric="MAE"),"\n")
mpause() # press enter
#

cat("forecast library methods:\n")
cat("HW MAE:",mmetric(Y,Pred,metric="MAE"),"\n")
cat("AR MAE:",mmetric(Y,Pred1,metric="MAE"),"\n")
cat("NN1 MAE:",mmetric(Y,Pred3,metric="MAE"),"\n")
cat("ET MAE:",mmetric(Y,Pred4,metric="MAE"),"\n")
cat("rminer NN methods:\n")
cat("NN2 MAE:",mmetric(Y,Pred5,metric="MAE"),"\n")
cat("LM MAE:",mmetric(Y,Pred6,metric="MAE"),"\n")