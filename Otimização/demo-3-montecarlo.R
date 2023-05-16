source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here

# dimension
D=7

# evaluation function:
  sphere=function(x) sum(x^2)

N=10000 # number of searches
# monte carlo search with D=2 and x in [-10.4,10.4]
lower=rep(0,D) # lower bounds
upper=rep(5,D) #  upper bounds
MC=mcsearch(fn=sphere,lower=lower,upper=upper,N=N,type="min")
cat("best solution:",MC$sol,"evaluation function",MC$eval," (found at iteration:",MC$index,")\n")
