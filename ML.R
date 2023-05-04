library(forecast) # access forecast functions -> HoltWinters, forecast
library(rminer) # access rminer functions -> CasesSeries, fit, lforecast, mmetric, mgraph, ...
library(readxl) 

# read data:
bebidas <- read_excel("C:/Users/Miguel Rebelo/Desktop/Projeto_HW/bebidas.xlsx")

cat("Choose the beer:\n 1-STELLA\n 2-BUD")

beer <- as.integer(readline());

switch(beer, 
       "1" = S <- ts(bebidas[,5]),
       "2"= S <- ts(bebidas[,6])
       ) 
    
# read the time series into object S
NPRED=7# number of predictions
srange=diff(range(S)) # compute the range of S

# show S and some statistics:
print(S)
print(summary(S))
cat("range:",srange,"\n")
cat("size:",length(S),"\n")
plot(S,type="l",col="blue")
acf(S) # autocorrelation plot
mpause() # rminer function, requires user to press enter

# CasesSeries: convert a single time series into a data.frame with inputs (...,lag2,lag1) and target output (y)
# selection of all 1 to 11 time lags:
D=CasesSeries(S,c(1:7)) # 11 time lags t-1,t-2,t-3,t-4,t-5,t-6,t-7,t-8,t-9,t-10,t-11 -> t
print(summary(D))
print("Show TR and TS indexes:")
N=nrow(D) # number of D examples
NTR=N-NPRED
TR=1:NTR # training row elements of D (oldest elements), excluding last NPRED rows
TS=(NTR+1):N #  test row elements of D (more recent elements), total of NPRED rows
print("TR:")
print(TR)
print("TS:")
print(TS)

model <- c("naive","mlp","cubist","pcr","randomforest","rvm","plsr","svm","mlpe","mars","cppls","naive","naivebayes","lr","xgboost","boosting","ctree","rpart","kknn")
cat("\n\n\nChoose ML model:\n1-naive\n2-mlp\n3-cubist\4-npcr\n5-randomforest\n6-rvm\n7-plsr\n8-svm\n9-mlpe\n10-mars\n11-cppls\n12-naive\n13-naivebayes\n14-lr\n15-xgboost\n16-boosting\n17-ctree\n18-rpart\n19-kknn")
i <- as.integer(readline());
#print(i)

# fit a Neural Network (NN) - multilayer perceptron ensemble with training data: 
mpause(paste("fit a neural network", model[i]))
NN=fit(y~.,D[TR,],model=model[i],search="heuristic")


# 1-ahead predictions:
print("Predictions (1-ahead):")

PNN=predict(NN,D[TS,])

# store the output target into object Y
Y=D[TS,]$y # real observed values

# show forecasting measures and graph:
cat(paste(model[i],"NN predictions:\n"))
print(PNN)
cat("MAE:",mmetric(Y,PNN,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,PNN,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,PNN,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,PNN,metric="RRSE"),"\n")


# graph: REG - simple Regression Plot
print("Graph with NN predictions (1-ahead):")
mgraph(Y,PNN,graph="REG",Grid=10,lty=1,col=c("black","blue"),main="NN predictions",leg=list(pos="topright",leg=c("target","predictions")))

