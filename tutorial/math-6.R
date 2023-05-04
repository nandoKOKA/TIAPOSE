library(rminer)

math=read.table(file="math2.csv",header=TRUE) 

# select inputs and output (regression):
inputs=2:29 
g3=which(names(math)=="G3")
cat("output class:",class(math[,g3]),"\n")
rmath=math[,c(inputs,g3)]
# for simplicity, this code file assumes a fit to all math data:
m=c("holdouto",2/3) # for internal validation: ordered holdout, 2/3 for training

# 10 searches for the mty randomForest parameter:
# after rminer 1.4.1, mparheuristic can be used:
s=list(search=mparheuristic("randomForest",n=10),method=m)
print("search values:")
print(s)
set.seed(123) # for replicability
R3=fit(G3~.,rmath,model="randomForest",search=s,fdebug=TRUE)
# show the automatically selected mtry value:
print(R3@mpar)

# same thing, but now with more verbose and using the full search parameter:
m=c("holdouto",2/3) # internal validation: ordered holdout, 2/3 for training
s=list(smethod="grid",search=list(mtry=1:10),convex=0,method=m,metric="SAE")
set.seed(123) # for replicability
R3b=fit(G3~.,rmath,model="randomForest",search=s,fdebug=TRUE)
print(R3b@mpar)
