source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here
#source("Optim.R") # a funcao de avaliação está definida aqui


# Restrições de distribuição máxima de bebidas
max_dist_v1 <- 60
max_dist_v2 <- 90
max_dist_v3 <- 120

# Restrição de empacotar e carregar até 72 bebidas por dia por recurso
max_pac_rec <- 72


#Vendas previstas - exemplo
##depois usar as previstas pelos metodos preditivos

vendas_previstas_stella=c(141, 154, 18, 102, 211, 69, 37)
vendas_previstas_bud=c(211, 172, 220, 330, 39, 45, 125)

# monte carlo search with D=7 and x in lower and upper
# Definição dos limites inferiores e superiores para cada variável
#arm v1 v2 v3 pac steella e pac bud
lower <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
upper <- c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500)

N=10000 # number of searches

montecarlo <- function(lower, upper, N) {
  # Create the vectors
  arm <- c(0, 0, 0, 0, 0, 0)
  v1 <- c(0, 0, 0, 0, 0, 0)
  v2 <- c(0, 0, 0, 0, 0, 0)
  v3 <- c(0, 0, 0, 0, 0, 0)
  pac_stella <- c(0, 0, 0, 0, 0, 0)
  pac_bud <- c(0, 0, 0, 0, 0, 0)
  
  
  # evaluation function:
  sphere <- function(x) sum(x^2)
  
  MC <- mcsearch(fn = sphere, lower = lower, upper = upper, N = N, type = "min")
  
  arm <- MC$sol[1:7]
  v1 <- MC$sol[8:14]
  v2 <- MC$sol[15:21]
  v3 <- MC$sol[22:28]
  pac_stella <- MC$sol[29:35]
  pac_bud <- MC$sol[36:42]
  
  s1 <- c(arm, v1, v2, v3, pac_stella, pac_bud)
  s1 <- verifica(s1)
  return(s1)
}


# Verificar se as restrições são satisfeitas
verifica <- function(s) {
    arm <- s[1:6]
    pac_stella <- s[29:35]
    pac_bud <- s[36:42]
  
  for (i in 1:7) {
    if (pac_stella[i] > max_dist_v1 || pac_stella[i] > max_dist_v2 || pac_stella[i] > max_dist_v3 || arm[i] > max_pac_rec || pac_bud[i] > max_dist_v1 || pac_bud[i] > max_dist_v2 || pac_bud[i] > max_dist_v3) {
      return(-Inf)
    } else {
      return(s)
    }
  }
}

s1 <- montecarlo(lower, upper, N)
print(s1)



