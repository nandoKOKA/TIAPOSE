source("hill.R") #  hclimbing is defined here
source("Optim.R") # eval está aqui


N=1000 # searches
REPORT=N/20 # report results


#set.seed(125)
# slight change of a real par under a normal u(0,0.5) function:
rchange1=function(par,lower,upper) # change for hclimbing
{ hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.25,round=FALSE) }



D=42 # dimension do nosso plano

lower <- c(-3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -500, -500, -500, -500, -500, -500, -500, -500, -500, -500, -500, -500, -500, -500)
upper <- c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500)

#lower=rep(-10.4,D) # lower bounds
#upper=rep(0,D) #  upper bounds
cat("hill climbing search eval (max profit) D=",D,"(iters=",N,")\n")

# initial solution: 
s0=rep(-500,D) # one extreme point, could be a random point
HC=hclimbing(par=s0,fn=eval,change=rchange1,lower=lower,upper=upper,type="max",
             control=list(maxit=N,REPORT=REPORT,digits=2))

#cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n")

sol=round(HC$sol)

cat("\n")
#imprimir a melhor solução do lucro
cat("arm:", sol[1:7], "\nv1:", sol[8:14], "\nv2:", sol[15:21], "\nv3:", sol[22:28], "\npac_stella:", sol[29:35], "\npac_bud:", sol[36:42])
#cat("best solution:",MC$sol,"evaluation function",MC$eval," (found at iteration:",MC$index,")\n")