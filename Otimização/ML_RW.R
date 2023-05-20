# 4-passengers.R: script demonstration of growing window and rolling window evaluations.

library(forecast) # access forecast functions -> HoltWinters, forecast
library(rminer) # access rminer functions -> CasesSeries, fit, lforecast, mmetric, mgraph, ...
library(readxl) 
# setwd() # adjust working directory if needed.

# read data:

bebidas <- read_excel("C:/Users/Miguel Rebelo/Desktop/TIAPOSE/Otimização/bebidas.xlsx")

cat("\nChoose the beer:\n 1-STEELA\n 2-BUDD")
beer <- as.integer(readline())#saves the type of beer

model <- c("naive","mlp","cubist","pcr","randomforest","rvm","plsr","svm","mlpe","mars","cppls","naive","naivebayes","lr","xgboost","boosting","ctree","rpart","kknn")
cat("\n\n\nChoose ML model:\n1-naive\n2-mlp\n3-cubist\4-npcr\n5-randomforest\n6-rvm\n7-plsr\n8-svm\n9-mlpe\n10-mars\n11-cppls\n12-naive\n13-naivebayes\n14-lr\n15-xgboost\n16-boosting\n17-ctree\n18-rpart\n19-kknn")
i <- as.integer(readline())# saves the position of the model



if (beer == 1) { # saves the name of the beer in use
  n_beer <- "STELLA"
  TS <- ts(bebidas[, 5])
} else {
  n_beer <- "BUD"
  TS <- ts(bebidas[, 6])
}


d1=TS # vector of numeric
L=length(d1) # size of the time series, 144
K=7# assumption for the seasonal period: test also acf(d1S)


print(paste("incremental (rowling) window training of",n_beer))
cat("\n")

Test=21 # H, the number of multi-ahead steps, adjust if needed
S=round(K/7) # K/3 step jump: set in this case to 4 months, a quarter
Runs=20 # number of growing window iterations, adjust if needed - 

# forecast:
W=(L-Test)-(Runs-1)*S # initial training window size for the ts space (forecast methods)

# rminer:
timelags=c(1:7) # 1 previous month, 12 and 13 previous year months, you can test other combinations, such as 1:13
D=CasesSeries(d1,timelags) # note: nrow(D) is smaller by max timelags than length(d1)
W2=W-max(timelags) # initial training window size for the D space (CasesSeries, rminer methods)

YR=diff(range(d1)) # global Y range, use the same range for the NMAE calculation in all iterations

ev=vector(length=Runs) # error vector for "HoltWinters"
ev2=vector(length=Runs) # error vector for "mlpe"

predicted_values <- vector(length = Test)
# rolling window:
for(b in 1:Runs)  # cycle of the incremental window training (growing window)
{
  # code for the forecast package methods, HoltWinters is just an example:
  H=holdout(d1,ratio=Test,mode="rolling",iter=b,window=W,increment=S)   
  trinit=H$tr[1]
  dtr=ts(d1[H$tr],frequency=K) # create ts object, note that there is no start argument (for simplicity of the code)
  M=suppressWarnings(HoltWinters(dtr)) # create forecasting model, suppressWarnings removes warnings from HW method
  Pred=forecast(M,h=length(H$ts))$mean[1:Test] # multi-step ahead forecasts
  ev[b]=mmetric(y=d1[H$ts],x=Pred,metric="NMAE",val=YR)
  
  # code for rminer package methods, "mlpe" is just an example:
  H2=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)   
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H2$tr[length(H2$tr)],]) # y is equal to previously shown value  
  M2=fit(y~.,D[H2$tr,],model[i]) # create forecasting model
  Pred2=lforecast(M2,D,start=(length(H2$tr)+1),Test) # multi-step ahead forecasts
  ev2[b]=mmetric(y=d1[H$ts],x=Pred2,metric="NMAE",val=YR)
  
  ############
  ################
  
  last_predicted_values <- tail(Pred2, 7)
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "nmae:",ev[b],",",ev2[b],"\n")
} # end of cycle

# show median of ev and ev2
print(paste("\nmedian NMAE values for HW and:",model[i]))
cat("\nHolt-Winters median NMAE:",median(ev),"\n")
print(paste(model[i],"median NMAE:",median(ev2)))
cat("\n")


#Mostar semana/dias previstos - esta a prever para a ultima semana
predicted_dates <- vector(length = K)
predicted_sales <- vector(length = K)
for(b in 1:Test) {
  predicted_dates[b] <- as.character(bebidas$DATA[H$ts[b]])
  predicted_dates <- predicted_dates[1:7]
  predicted_sales[b] <- as.character(H$ts[b])
  predicted_sales <- predicted_sales[1:7]

  }
print("Predicted Dates:")
print(predicted_dates)
print(predicted_sales)


print(last_predicted_values)


# last iteration predictions:
mgraph(d1[H$ts],Pred,graph="REG",Grid=10,col=c("black","blue","red"),leg=list(pos="topleft",leg=c("target","HW pred.","mlpe")))
lines(Pred2,pch=19,cex=0.5,type="b",col="red")