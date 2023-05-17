source("Optim.R")

# monte carlo search with D=7 and x in lower and upper
# Definição dos limites inferiores e superiores para cada variável
# arm v1 v2 v3 pac steella e pac bud
lower <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
upper <- c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500)

N <- 10000 # number of searches

#montecarlo para maximizar o lucro 
# funcao eval  - retorna o lucro
MC=mcsearch(eval,lower=lower,upper=upper,N=N,type="max")
sol=round(MC$sol)

#imprimir a melhor solução do lucro
cat("arm:", sol[1:7], "\nv1:", sol[8:14], "\nv2:", sol[15:21], "\nv3:", sol[22:28], "\npac_stella:", sol[29:35], "\npac_bud:", sol[36:42])
#cat("best solution:",MC$sol,"evaluation function",MC$eval," (found at iteration:",MC$index,")\n")


