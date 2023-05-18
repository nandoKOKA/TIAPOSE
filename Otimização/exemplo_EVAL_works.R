#Exemplo Eval Works
source("Eval.R")

#Declarar e inicializar variaveis que o repair precisa
max_pac_rec <- 72
max_dist_v1 <- 60
max_dist_v2 <- 90
max_dist_v3 <- 120


#construir vetores do plano invalido
arm=c(6, 3, 0, 1, 1, 0, 1)
v1 <- c(2, 0, 1, 0, 2, 0, 0)
v2 <- c(1, 0, 0, 1, 0, 0, 0)
v3 <- c(1, 0, 1, 0, 0, 1, 0)
pac_stella <- c(100, 0, 45, 66, 0, 0, 0)
pac_bud <- c(200, 0, 0, 52, 0, 78, 0)

#guardar plano 
s1 <- c(arm, v1, v2, v3, pac_stella, pac_bud)

#ver lucro do plano 
print(eval(s1))