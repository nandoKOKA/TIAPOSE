source("hill.R")
source("Eval.R") # eval está aqui

# dimension
D=42

#abordagem stor
#total de cervejas
tc = vendas_previstas_bud + vendas_previstas_stella
vtc = 0.35 * tc # das previtas, usamos apenas 35% - dependo da confianca das previsoes
n_func_max= round(vtc/72) # saber o nmr de funcionarios necessários


# hill climbing search
N=1000 # 100 searches
REPORT=N/10 # report results


lower <- c(rep(0, 7), rep(0, 7), rep(0, 7), rep(0, 7), rep(0, 7), rep(0,7))
upper <- c(n_func_max, rep(3, 7), rep(3, 7), rep(3, 7), rep(500, 7), rep(500, 7))


# slight change of a real par under a normal u(0,0.5) function:
rchange2=function(par) # change for hclimbing
{ hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.5,round=FALSE) }


cat("Simulated Annealing search max profit D=",D,"(iters=",N,")\n")
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

#imprimir a melhor solução do lucro
cat("\nBest solution:\n")
cat("arm:", sol[1:7], "\nv1:", sol[8:14], "\nv2:", sol[15:21], "\nv3:", sol[22:28], "\npac_stella:", sol[29:35], "\npac_bud:", sol[36:42])
cat("\nLucro:",SA$value,"\n")