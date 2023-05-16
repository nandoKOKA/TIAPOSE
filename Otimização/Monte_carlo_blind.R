# Restrições de distribuição máxima de bebidas
max_dist_v1 <- 60
max_dist_v2 <- 90
max_dist_v3 <- 120

# Restrição de empacotar e carregar até 72 bebidas por dia por recurso
max_pac_rec <- 72

#Vendas previstas - exemplo
##depois usar as previstas pelos metodos preditivos
vendas_previstas_stella <- c(141, 154, 18, 102, 211, 69, 37)
vendas_previstas_bud <- c(211, 172, 220, 330, 39, 45, 125)

# monte carlo search with D=7 and x in lower and upper
# Definição dos limites inferiores e superiores para cada variável
# arm v1 v2 v3 pac steella e pac bud
lower <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
upper <- c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500)


N <- 10000 # number of searches

montecarlo <- function(lower, upper, N) {
  # Create the vectors
  arm <- c(0, 0, 0, 0, 0, 0)
  v1 <- c(0, 0, 0, 0, 0, 0)
  v2 <- c(0, 0, 0, 0, 0, 0)
  v3 <- c(0, 0, 0, 0, 0, 0)
  pac_stella <- c(0, 0, 0, 0, 0, 0)
  pac_bud <- c(0, 0, 0, 0, 0, 0, 0)
               
               # evaluation function:
               sphere <- function(x) sum(x^2)
               
               search <- matrix(runif(N*length(lower), lower, upper), ncol = length(lower), byrow = TRUE)
               result <- fsearch(search, fn = sphere, type = "min")
               
               arm <- result$sol[1:7]
               v1 <- result$sol[8:14]
               v2 <- result$sol[15:21]
               v3 <- result$sol[22:28]
               pac_stella <- result$sol[29:35]
               pac_bud <- result$sol[36:42]
               
               s1 <- c(arm, v1, v2, v3, pac_stella, pac_bud)
               
               #return(s1)
}



# Verificar se as restrições são satisfeitas
repair <- function(s) {
  s=round(s)
  
  for (i in 1:7) {
    ## tratar veiculos e bebidas empacotadas
    
    if (
      pac_stella[i] > (arm[i] * max_pac_rec) ||
      pac_bud[i] > (arm[i] * max_pac_rec))
      {
      pac_stella[i] = pac_stella[i] / max_pac_rec
      
    } else if (pac_stella[i] > ((v1[i]*max_dist_v1)  + (v2[i]*max_dist_v2) + (v3[i]*max_dist_v3)) ) {
      
       resto <- pac_stella[i] - ((v1[i]*max_dist_v1)  + (v2[i]*max_dist_v2) + (v3[i]*max_dist_v3))
       
       while(resto != 0){
         
           if(resto <60){
             v1[i] = v1[i] + 1
             resto = resto - 60
             
           } else if (resto<90) {
             v2[i] = v2[i] + 1
             resto= restp - 90
             
           } else if(resto<120) {
             v3[i] = v[3] + 1
             resto= resto - 90
           }
           
          else { ## quando é maior que 120 adiciona logo um v3
              v3[i] = v3[i] + 1
              resto= resto - 120  #atualizar o resto
          }
    }
    
  }
      
  }
  arm <- s[1:7]
  v1 <- s[8:14]
  v2 <- s[15:21]
  v3 <- s[22:28]
  pac_stella <- s[29:35]
  pac_bud <- s[36:42]
  
  s1 <- c(arm, v1, v2, v3, pac_stella, pac_bud)
  #return(s1)
}  

s1 <- montecarlo(lower, upper, N)
s2 <- repair(s1)



#Imprimir ultimo plano
cat("arm:", s2[1:7], "\nv1:", s2[8:14], "\nv2:", s2[15:21], "\nv3:", s2[22:28], "\npac_stella:", s2[29:35], "\npac_bud:", s2[35:42])

