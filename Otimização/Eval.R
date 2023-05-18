#Definir os custos fixos e outras variaveis 
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
custo_stock_stella <-0
custo_stock_bud <-0
recursos <-0
custo_total_arm <-0
custo_total_vei <- 0
custo_total_vei_1 <-0
custo_total_vei_2 <-0
custo_total_vei_3 <-0
lucro_vendas_st_bud <- 0
custo_total_empresa <- 0 


# definir a função eval
eval <- function(s) {
  
  
  s=abs(round(s))
  s=repair(s) #função que vai corrigir os valores
  
  arm <- s[1:7]
  v1 <- s[8:14]
  v2 <- s[15:21]
  v3 <- s[22:28]
  pac_stella <- s[29:35]
  pac_bud <- s[36:42]
  
  for(i in 1:7) {
    
    # criar vetores de vendas e stock finais
    #STELLA
    if(pac_stella[i]>vendas_previstas_stella[i]){
      vendas_stella[i]= vendas_previstas_stella[i] #pac stella
      stock_stella[i]= pac_stella[i] - vendas_previstas_stella[i]
    } else if(i!=1) {
      vendas_stella[i]= pac_stella[i] + stock_stella[i-1] 
      stock_stella[i]= 0
    } else {
      vendas_stella[i]= pac_stella[i]
      stock_stella[i]= 0
    }
    
    #BUD
    if(pac_bud[i]>vendas_previstas_bud[i]){
      vendas_bud[i]= vendas_previstas_bud[i] #pac bud
      stock_bud[i]= pac_bud[i] - vendas_previstas_bud[i]
    } else if(i!=1) {
      vendas_bud[i]= pac_bud[i] + stock_bud[i-1] 
      stock_bud[i]= 0
    } else {
      vendas_bud[i]= pac_bud[i]
      stock_bud[i]= 0
    }
    
    # calcula o custo total do armazem e veiculos
    
    #fins de semana
    if( i >= 2 & i <= 3){
      custo_total_arm = custo_total_arm + (arm[i] * custo_armazem_fim_semana)
      custo_total_vei_1 = custo_total_vei_1 + (v1[i] * custo_distribuicao_v1_fim_semana)
      custo_total_vei_2 = custo_total_vei_2 + (v2[i] * custo_distribuicao_v2_fim_semana)
      custo_total_vei_3 = custo_total_vei_3 + (v3[i] * custo_distribuicao_v3_fim_semana)
    } else {
      #semana
      custo_total_arm = custo_total_arm + (arm[i] * custo_armazem_normal)
      custo_total_vei_1 = custo_total_vei_1 + (v1[i] * custo_distribuicao_v1_normal)
      custo_total_vei_2 = custo_total_vei_2 + (v2[i] * custo_distribuicao_v2_normal)
      custo_total_vei_3 = custo_total_vei_3 + (v3[i] * custo_distribuicao_v3_normal)
    }
    
    #calcular custo stock
    custo_stock_stella = custo_stock_stella + stock_stella[i]
    custo_stock_bud = custo_stock_bud + stock_bud[i]
    custo_total_stock_imperiais = custo_stock_bud + custo_stock_stella
    
    #calcular lucro de vendas
    lucro_vendas_st_bud =  lucro_vendas_st_bud + ((vendas_bud[i] * preco_bud) + (vendas_stella[i] * preco_stella)) 
    
    #calcular custos
    custo_total_vei = custo_total_vei_1 + custo_total_vei_2 + custo_total_vei_3
    custo_total_empresa = custo_total_arm + custo_total_vei + custo_total_stock_imperiais
    despesas = custo_total_empresa
    
    #calcular lucro final 
    lucro = lucro_vendas_st_bud - despesas
    recursos =  recursos + (arm[i]+v1[i]+v2[i]+v3[i])
    
    #atualizar s1
    s1=c(arm,v1,v2,v3,pac_stella,pac_bud)
    
  }
  
  if(lucro<0) {
    lucro=abs(lucro)
  }
  #hide
  if(FALSE) {
    print(noquote(paste("recursos =", recursos[1])))
    print(noquote(paste("custo vei =", custo_total_vei)))
    print(noquote(paste("custo arm =", custo_total_arm)))
    print(noquote(paste("lucro vendas =", lucro_vendas_st_bud)))
    
  }
  return(lucro)
}