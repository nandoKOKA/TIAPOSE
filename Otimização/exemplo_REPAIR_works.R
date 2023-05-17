#TESTAR SE REPAIR RESOLVE SOLUÇÕES QUE NÃO SÃO VÁLIDAS
# NESTE CASO o plano s1 contem menos veículos que os necessários para distribuir as bebidas do primeiro dia
# logo o repair terá que aumentar o númenro de veiculos 

source("Repair.R")

#Declarar e inicializar variaveis que o repair precisa
max_pac_rec <- 72
max_dist_v1 <- 60
max_dist_v2 <- 90
max_dist_v3 <- 120


#construir vetores do plano invalido
arm=c(6, 3, 0, 1, 1, 0, 1)
v1 <- c(2, 0, 1, 0, 2, 0, 0)
v2 <- c(1, 0, 0, 1, 0, 0, 0)
v3 <- c(0, 0, 1, 0, 0, 0, 0)
pac_stella <- c(100, 0, 45, 66, 0, 0, 0)
pac_bud <- c(200, 0, 0, 52, 0, 78, 0)

#guardar plano invalido
s1 <- c(arm, v1, v2, v3, pac_stella, pac_bud)


#guardar plano válido
s2 <- round(repair(s1))

#Imprimir plano invalido plano
cat("\nPlano inválido:")
cat("\narm:", s1[1:7], "\nv1:", s1[8:14], "\nv2:", s1[15:21], "\nv3:", s1[22:28], "\npac_stella:", s1[29:35], "\npac_bud:", s1[36:42])
cat("\n\nPlano válido:")
cat("\n")

#Imprimir plano valido
cat("arm:", s2[1:7], "\nv1:", s2[8:14], "\nv2:", s2[15:21], "\nv3:", s2[22:28], "\npac_stella:", s2[29:35], "\npac_bud:", s2[36:42])

