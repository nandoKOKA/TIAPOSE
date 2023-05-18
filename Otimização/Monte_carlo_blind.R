source("Eval.R")

# monte carlo search with D=7 and x in lower and upper
# Definição dos limites inferiores e superiores para cada variável
# arm v1 v2 v3 pac steella e pac bud

#total de cervejas
tc = vendas_previstas_bud + vendas_previstas_stella
vtc = 0.35 * tc # das previtas, usamos apenas 35% - dependo da confianca das previsoes
n_func_max= round(vtc/72) # saber o nmr de funcionarios necessários


lower <- c(rep(0, 7), rep(0, 7), rep(0, 7), rep(0, 7), rep(0, 7), rep(0,7))
upper <- c(n_func_max, rep(3, 7), rep(3, 7), rep(3, 7), rep(500, 7), rep(500, 7))


N <- 10000 # number of searches

#montecarlo para maximizar o lucro 
# funcao eval  - retorna o lucro
MC=mcsearch(eval,lower=lower,upper=upper,N=N,type="max")
sol=round(MC$sol)

#imprimir a melhor solução do lucro
cat("\nBest solution:\n")
cat("arm:", sol[1:7], "\nv1:", sol[8:14], "\nv2:", sol[15:21], "\nv3:", sol[22:28], "\npac_stella:", sol[29:35], "\npac_bud:", sol[36:42])
cat("\nEvaluation function:",MC$eval," (found at iteration:",MC$index,")\n")


