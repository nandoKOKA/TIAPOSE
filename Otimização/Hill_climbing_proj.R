source("hill.R") #  hclimbing is defined here
source("Eval.R") # eval está aqui 

N=5000 # searches
REPORT=N/20 # report results


#set.seed(125)
# slight change of a real par under a normal u(0,0.5) function:
rchange1=function(par,lower,upper) # change for hclimbing
{ hchange(par,lower=lower,upper=upper,rnorm,mean=45,sd=0.90,round=FALSE) }



D=42 # dimension do nosso plano

#abordagem stor
#total de cervejas
tc = vendas_previstas_bud + vendas_previstas_stella
vtc = 0.35 * tc # das previtas, usamos apenas 35% - dependo da confianca das previsoes
n_func_max= round(vtc/72) # saber o nmr de funcionarios necessários


lower <- c(rep(0, 7), rep(0, 7), rep(0, 7), rep(0, 7), rep(0, 7), rep(0,7))
upper <- c(n_func_max, rep(3, 7), rep(3, 7), rep(3, 7), rep(500, 7), rep(500, 7))

#lower=rep(-10.4,D) # lower bounds
#upper=rep(0,D) #  upper bounds
cat("hill climbing search eval (max profit) D=",D,"(iters=",N,")\n")

# initial solution: 
s0=rep(0,D) # one extreme point, could be a random point
HC=hclimbing(par=s0,fn=eval,change=rchange1,lower=lower,upper=upper,type="max",
             control=list(maxit=N,REPORT=REPORT,digits=2))

#cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n")

cat("\n")

# Print the best solution (quantities)
sol <- round(HC$sol)
arm <- sol[1:7]
v1 <- sol[8:14]
v2 <- sol[15:21]
v3 <- sol[22:28]
pac_stella <- sol[29:35]
pac_bud <- sol[36:42]

#imprimir a melhor solução do lucro
cat("\nBest solution:\n")
cat("arm:", sol[1:7], "\nv1:", sol[8:14], "\nv2:", sol[15:21], "\nv3:", sol[22:28], "\npac_stella:", sol[29:35], "\npac_bud:", sol[36:42])
cat("\nLucro:",HC$eval,"\n")