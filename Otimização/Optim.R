source("Repair.R")
source("Eval.R")

#Definir os custos fixos
custo_armazem_normal <- 10
custo_armazem_fim_semana <- 15
custo_distribuicao_v1_normal <- 40
custo_distribuicao_v2_normal <- 50
custo_distribuicao_v3_normal <- 53
custo_distribuicao_v1_fim_semana <- 45
custo_distribuicao_v2_fim_semana <- 55
custo_distribuicao_v3_fim_semana <- 58
max_produtos_v1 <- 60
max_produtos_v2 <- 90
max_produtos_v3 <- 120
custo_armazem_refrigeracao <- 1
vendas_stella <-0
vendas_bud <-0
custo_veiculos <-0
custo_stock_stella <-0
custo_stock_bud <-0
lucro_final <-0
recursos <-0
custo_total_arm <-0
custo_total_vei <- 0
custo_total_vei_1 <-0
custo_total_vei_2 <-0
custo_total_vei_3 <-0
lucro_vendas_st_bud <- 0
custo_total_empresa <- 0 
# lucro das bebidas vendidas no mesmo dia
preco_stella <- 5.7
preco_bud <- 4.4

# Definir os vetores exemplo
vendas_previstas_stella=c(141, 154, 18, 102, 211, 69, 37)
vendas_previstas_bud=c(211, 172, 220, 330, 39, 45, 125)
arm=c(6, 3, 0, 1, 1, 0, 1)
v1=c(2, 0, 0, 1, 0, 0, 0)
v2=c(2, 1, 0, 0, 1, 0, 0)
v3=c(2, 1, 0, 0, 0, 0, 0)

#Definir os vetores das bebidas empacotadas e distribuidas
pac_stella=c(160, 8, 0, 52, 20, 0, 0)
pac_bud=c(200, 200, 0, 0, 30, 0, 0)


#criar array para o stock de steella e bud
stock_stella=c(0, 0, 0, 0, 0, 0, 0)
stock_bud=c(0, 0, 0, 0, 0, 0, 0)

#Criar as vendas de cada cerveja
vendas_stella=c(0, 0, 0, 0, 0, 0, 0)
vendas_bud=c(0, 0, 0, 0, 0, 0, 0)

# Definir Plano
s1=c(arm,v1,v2,v3,pac_stella,pac_bud)

#imprimir lucro e testar funcao
print(noquote(paste("LUCRO FINAL =", eval(s1))))

