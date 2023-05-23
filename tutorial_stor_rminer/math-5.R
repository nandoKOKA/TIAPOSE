library(rminer)

math=read.table(file="math2.csv",header=TRUE) 

# select inputs and output (regression):
inputs=2:29; g3=which(names(math)=="G3")
rmath=math[,c(inputs,g3)]
# for simplicity, this code file assumes a fit to all math data:

print("examples that set some parameters to fixed values:")

print("mlp model with decay=0.1:")
R4=fit(G3~.,rmath,model="mlp",decay=0.1)
print(R4@mpar)

print("rpart with minsplit=10")
R5=fit(G3~.,rmath,model="rpart",control=rpart::rpart.control(minsplit=10))
print(R5@mpar)
print("rpart with minsplit=10 (simpler fit code)")
R5b=fit(G3~.,rmath,model="rpart",control=list(minsplit=10))
print(R5b@mpar)

print("ksvm with kernel=vanilladot and C=10")
R6=fit(G3~.,rmath,model="ksvm",kernel="vanilladot",C=10)
print(R6@mpar)

print("ksvm with kernel=tanhdot, scale=2 and offset=2")
# fit already has a scale argument, thus the only way to fix scale of "tanhdot"
# is to use the special search argument with the "none" method:
s=list(smethod="none",search=list(scale=2,offset=2))
R7=fit(G3~.,rmath,model="ksvm",kernel="tanhdot",search=s)
print(R7@mpar)
