semana_selecionada_ML_RW <- function (SemanaSelecionada,cerveja) {
    #STEELLA - MELHOR MÉTODO MLPE
    
    ##Enviar as vendas previstas Stella e bud consoante semana
    
    
    library(forecast) # access forecast functions -> HoltWinters, forecast
    library(rminer) # access rminer functions -> CasesSeries, fit, lforecast, mmetric, mgraph, ...
    library(readxl) 
    # setwd() # adjust working directory if needed.
    
    # read data:
    
    bebidas <- read_excel("C:/Users/Miguel Rebelo/Desktop/TIAPOSE/Otimização/bebidas.xlsx")
    
    if(cerveja == "steella") {
      TS <- ts(bebidas[,5]) # STEELLA
      
    } else{
      TS <- ts(bebidas[,6]) # BUD
    }
    
    
    d1=TS # vector of numeric
    L=length(d1) # size of the time series, 144
    K=7# assumption for the seasonal period: test also acf(d1S)
    
    Test=(7*SemanaSelecionada) # H, the number of multi-ahead steps, adjust if needed
    S=round(K/7) # K/3 step jump: set in this case to 4 months, a quarter
    Runs=20 # number of growing window iterations, adjust if needed - 
    
    # forecast:
    W=(L-Test)-(Runs-1)*S # initial training window size for the ts space (forecast methods)
    
    # rminer:
    timelags=c(1:7) # 1 previous month, 12 and 13 previous year months, you can test other combinations, such as 1:13
    D=CasesSeries(d1,timelags) # note: nrow(D) is smaller by max timelags than length(d1)
    W2=W-max(timelags) # initial training window size for the D space (CasesSeries, rminer methods)
    
    YR=diff(range(d1)) # global Y range, use the same range for the NMAE calculation in all iterations
    
    
    predicted_sales <- vector(length = Test)
    
    # rolling window:
    for(b in 1:Runs)  # cycle of the incremental window training (growing window)
    {
      
      # code for rminer package methods, "mlpe" is just an example:
      H2=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)   
      # note: the last training value is the same for dtr, namely:
      # print(dtr[length(dtr)])  
      # print(D[H2$tr[length(H2$tr)],]) # y is equal to previously shown value  
      M2=fit(y~.,D[H2$tr,],"mlpe") # create forecasting model
      Pred2=lforecast(M2,D,start=(length(H2$tr)+1),Test) # multi-step ahead forecasts
      
      
      #guardar previsoes
      predicted_sales <- tail(Pred2, 7)
      
    } # end of cycle
    
   
    #Mostar semana/dias previstos - esta a prever para a ultima semana
    predicted_dates <- vector(length = K)
    
    
    
    for(b in 1:Test) {
      predicted_dates[b] <- as.character(bebidas$DATA[H2$ts[b]])
      
    }
    
    predicted_dates <- predicted_dates[1:7]
    
    print(predicted_sales)
    resultado <- list(predicted_dates = predicted_dates[1:7], predicted_sales)
    
    return(resultado)
    
}




