semana_selecionada_ML_RW <- function(SemanaSelecionada, cerveja,metodo_ml) {
  library(forecast)
  library(readxl)
  library(rminer)
  
  bebidas <- read_excel("~/Documents/GitHub/TIAPOSE/Otimização/bebidas.xlsx")
  
  if (cerveja == "steella") {
    TS <- ts(bebidas[, 5])
  } else {
    TS <- ts(bebidas[, 6])
  }
  
  d1 <- TS
  L <- length(d1)
  K <- 7
  Test <- 7 * SemanaSelecionada
  S <- round(K / 7)
  Runs <- 20
  
  W <- (L - Test) - (Runs - 1) * S
  
  timelags <- c(1:7)
  D <- CasesSeries(d1, timelags)
  W2 <- W - max(timelags)
  
  YR <- diff(range(d1))
  
  predicted_sales <- vector(length = Test)
  
  H2 <- NULL
  
  for (b in 1:Runs) {
    H2 <- holdout(D$y, ratio = Test, mode = "incremental", iter = b, window = W2, increment = S)
    M2 <- fit(y ~ ., D[H2$tr, ], metodo_ml)
    Pred2 <- lforecast(M2, D, start = (length(H2$tr) + 1), Test)
    
    predicted_sales[(b - 1) * S + 1:b * S] <- Pred2
  }
  
  predicted_dates <- vector(length = K)
  
  for (b in 1:Test) {
    predicted_dates[b] <- as.character(bebidas$DATA[H2$ts[b]])
  }
  
  predicted_dates <- predicted_dates[1:7]
  predicted_sales_c <- predicted_sales[1:7]
  
  
  resultado <- list(predicted_dates = predicted_dates, predicted_sales = predicted_sales_c)
  
  return(resultado)
}



Otimiza <-function (predicted_sales_steella, predicted_sales_bud) {
  
  source("hill.R")
  source("eval_despesas.R")
  source("Eval.R")
  
  
  #guardar vendas
  vendas_previstas_stella <- predicted_sales_steella
  vendas_previstas_bud <- predicted_sales_bud
  
  print("Vendas pre st e bud:")
  print(vendas_previstas_stella)
  print(vendas_previstas_bud)
  
  # dimension
  D=42
  
  #abordagem stor
  #Para ter o lower value do armazem podemos calcular os recursos com base
  #numa percentagem das vendas previstas
  #total de cervejas
  tc = vendas_previstas_stella + vendas_previstas_bud
  vtc = 0.50 * tc # das previtas, usamos apenas 35% - dependendo da confianca das previsoes
  n_func_max= round(vtc/72) # saber o nmr de funcionarios necessários
  
  
  ##podemos utilizar tambem a mesma abordagem para os veiculos
  n_vei_max_v1 =round(vtc/60)
  n_vei_max_v2 =round(vtc/90)
  n_vei_max_v3 =round(vtc/120)
  
  
  
  # hill climbing search
  N=1000 # 100 searches
  REPORT=N/10 # report results
  
  #Assumir por ex que temos 3 vei de cada tipo para cada dia da semana - 9 veiculos de transporte
  lower <- c(rep(0, 7), rep(0, 7), rep(0, 7), rep(0, 7), rep(0, 7), rep(0,7))
  upper <- c(n_func_max, n_vei_max_v1, n_vei_max_v2, n_vei_max_v3, rep(900, 7), rep(900, 7))
  
  
  # slight change of a real par under a normal u(0,0.5) function:
  rchange2=function(par) # change for hclimbing
  { hchange(par,lower=lower,upper=upper,rnorm,mean=45,sd=0.90,round=FALSE) }
  
  
  #cat("Simulated Annealing search max profit D=",D,"(iters=",N,")\n")
  CSANN=list(maxit=N,temp=5,trace=TRUE)
  SA=optim(par=rep(0,D),fn=eval,method="SANN",gr=rchange2,control=CSANN)
  
  cat("\n")
  
  # Print the best solution (quantities)
  sol <- round(SA$par)
  arm <- sol[1:7]
  v1 <- sol[8:14]
  v2 <- sol[15:21]
  v3 <- sol[22:28]
  pac_stella <- sol[29:35]
  pac_bud <- sol[36:42]
  
  
  #pegar custos da func eval_despesas
  resultado_eval <- eval_despesas(sol)  # Chamada da função eval
  despesas <- resultado_eval[2]
 
 
  if(FALSE){
  #imprimir a melhor solução do lucro
  cat("\nBest solution:\n")
  cat("arm:", sol[1:7], "\nv1:", sol[8:14], "\nv2:", sol[15:21], "\nv3:", sol[22:28], "\npac_stella:", sol[29:35], "\npac_bud:", sol[36:42])
  cat("\nLucro:",abs(SA$value),"\n")
  }
  
  lucro_final <- abs(SA$value)
  resultado <- list(arm,v1, v2, v3, pac_stella,pac_bud,lucro_final, despesas)
  
  print(resultado)
  return(resultado)
}
