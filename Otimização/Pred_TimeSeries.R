# Load libraries and data
library(forecast)
library(rminer)
library(readxl)

#Load dataset
bebidas <- read_excel("C:/Users/Miguel Rebelo/Desktop/Projeto_HW/bebidas.xlsx")

cat("Choose the beer:\n 1-STEELA\n 2-BUDD")

beer <- as.integer(readline());

switch(beer, 
       "1" = TS <- ts(bebidas[,5]), #CRIAR TS COM DADOS DE STELLA,                           
       "2"= TS <- ts(bebidas[,6]) #CRIAR TS COM DADOS DE BUDD
) 

if(beer==1) {
  n_beer ="STELLA"
} else {
  n_beer="BUD"
}
  
  

  L=length(TS)
  K=7 # TS period (weekly)
  print(paste("show graph of", n_beer))
  tsdisplay(TS)
  mpause()
  
  #LTR
  #L
  NTS=K # number of predictions
  H=2*K ## H = NTS || prever semana 19 || prever semana 18 - 3*K || ...
  
  LTR=L-H
  
  TR=ts(TS[1:LTR],frequency=K) #eliminar
  
  plot(TR)
  print(TR)
  mpause()
  
  
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
  cat("NMAE:",mmetric(Y,Pred,metric="NMAE"),"\n")
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
  cat("NMAE:",mmetric(Y,Pred1,metric="NMAE"),"\n")
  mpause() # press enter
  
  # NN from forecast:
  print("model> nnetar")
  NN1=nnetar(TR,P=1,repeats=3)
  print(NN1)
  F3=forecast(NN1,h=H)
  Pred3=F3$mean[1:H] # HolWinters format
  mgraph(Y,Pred3,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","NN1 pred.")))
  cat("NMAE:",mmetric(Y,Pred3,metric="NMAE"),"\n")
  mpause() # press enter
  
  # ets from forecast:
  print("model> ets")
  ETS=ets(TR)
  F4=forecast(ETS,h=H)
  Pred4=F4$mean[1:H] # HolWinters format
  mgraph(Y,Pred4,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","ets pred.")))
  cat("NMAE:",mmetric(Y,Pred4,metric="NMAE"),"\n")
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
  cat("NMAE:",mmetric(Y,Pred5,metric="NMAE"),"\n")
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
  cat("NMAE:",mmetric(Y,Pred5,metric="NMAE"),"\n")
  mpause() # press enter
  #
  
  cat("forecast library methods:\n")
  cat("HW NMAE:",mmetric(Y,Pred,metric="NMAE"),"\n")
  cat("AR NMAE:",mmetric(Y,Pred1,metric="NMAE"),"\n")
  cat("NN1 NMAE:",mmetric(Y,Pred3,metric="NMAE"),"\n")
  cat("ET NMAE:",mmetric(Y,Pred4,metric="NMAE"),"\n")
  cat("\nrminer NN methods:\n")
  cat("NN2 NMAE:",mmetric(Y,Pred5,metric="NMAE"),"\n")
  cat("LM NMAE:",mmetric(Y,Pred6,metric="NMAE"),"\n")
  







