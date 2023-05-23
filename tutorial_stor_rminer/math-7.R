#library(rminer)
#math=read.table(file="math2.csv",header=TRUE) 

# select inputs and output (regression):
inputs=2:29; g3=which(names(math)=="G3")
rmath=math[,c(inputs,g3)]
# for simplicity, this code file assumes a fit to all math data:

mint=c("kfold",3,123) # internal 3-fold, same seed

print("more sophisticated examples for setting hyperparameters:")

cat("mlpe model, grid for hidden nodes (size):",seq(0,8,2),"\n")
s=list(smethod="grid",search=list(size=seq(0,8,2)),method=mint,convex=0)
R9=fit(G3~.,rmath,model="mlpe",decay=0.1,maxit=25,nr=5,search=s,fdebug=TRUE)
print(R9@mpar)

cat("mlpe model, same grid using mparheuristic function:",seq(0,8,2),"\n")
s=list(search=mparheuristic("mlpe",lower=0,upper=8,by=2),method=mint)
R9b=fit(G3~.,rmath,model="mlpe",decay=0.1,maxit=25,nr=5,search=s,fdebug=TRUE)
print(R9b@mpar)

cat("mlpe model, grid for hidden nodes:",1:2,"x decay:",c(0,0.1),"\n")
s=list(smethod="grid",search=list(size=1:2,decay=c(0,0.1)),method=mint,convex=0)
R9c=fit(G3~.,rmath,model="mlpe",maxit=25,search=s,fdebug=TRUE)
print(R9c@mpar)

cat("mlpe model, same search but with matrix method:\n")
s=list(smethod="matrix",search=list(size=rep(1:2,times=2),decay=rep(c(0,0.1),each=2)),method=mint,convex=0)
R9d=fit(G3~.,rmath,model="mlpe",maxit=25,search=s,fdebug=TRUE)
print(R9d@mpar)

# 2 level grid with total of 8 searches 
#  note of caution: some "2L" ranges may lead to non integer (e.g. 1.3) values at
#  the 2nd level search. And some R functions crash if non integer values are used for
#  integer parameters.
cat("mlpe model, 2L search for size:\n")
s=list(smethod="2L",search=list(size=c(4,8,12,16)),method=mint,convex=0)
R9d=fit(G3~.,rmath,model="mlpe",maxit=25,search=s,fdebug=TRUE)
print(R9d@mpar)

print("ksvm with kernel=rbfdot: sigma, C and epsion (3^3=27 searches):")
s=list(smethod="grid",search=list(sigma=2^c(-8,-4,0),C=2^c(-1,2,5),epsilon=2^c(-9,-5,-1)),method=mint,convex=0)
R10=fit(G3~.,rmath,model="ksvm",kernel="rbfdot",search=s,fdebug=TRUE)
print(R10@mpar)

# even rpart or ctree parameters can be searched:
# example with rpart and cp:
print("rpart with control= cp in 10 values in 0.01 to 0.18 (10 searches):")
s=list(search=mparheuristic("rpart",n=10,lower=0.01,upper=0.18),method=mint)
R11=fit(G3~.,rmath,model="rpart",search=s,fdebug=TRUE)
print(R11@mpar)

# same thing, but with more explicit code that can be adapted for
# other rpart arguments, since mparheuristic only works for cp:
# a vector list needs to be used for the search$search parameter
print("rpart with control= cp in 10 values in 0.01 to 0.18 (10 searches):")
# a vector list needs to be used for putting 10 cp values
lcp=vector("list",10) # 10 grid values for the complexity cp
names(lcp)=rep("cp",10) # same cp name 
scp=seq(0.01,0.18,length.out=10) # 10 values from 0.01 to 0.18
for(i in 1:10) lcp[[i]]=scp[i] # cycle needed due to [[]] notation
s=list(smethod="grid",search=list(control=lcp),method=mint,convex=0)
R11b=fit(G3~.,rmath,model="rpart",search=s,fdebug=TRUE)
print(R11b@mpar)

# check ?rminer::fit for further examples
