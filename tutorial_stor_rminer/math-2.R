library(rminer) 

# read previously saved file
math=read.table(file="math2.csv",header=TRUE) 

# select inputs:
inputs=2:29 # select from 2 ("sex") to 29 ("health")

# select outputs: binary task "pass"
bout=which(names(math)=="pass")
cat("output class:",class(math[,bout]),"\n")

# two white-box examples:
B1=fit(pass~.,math[,c(inputs,bout)],model="rpart") # fit a decision tree
print(B1@object)
pdf("trees-1.pdf")
# rpart functions:
plot(B1@object,uniform=TRUE,branch=0,compress=TRUE)
text(B1@object,xpd=TRUE,fancy=TRUE,fwidth=0.2,fheight=0.2)
dev.off()

B2=fit(pass~.,math[,c(inputs,bout)],model="ctree") # fit a conditional inference tree
print(B1@object)
pdf("trees-2.pdf")
# ctree function:
plot(B2@object) 
dev.off()

# two black-box examples:
B3=fit(pass~.,math[,c(inputs,bout)],model="mlpe") # fit a multilayer perceptron ensemble
print(B3@object)

B4=fit(pass~.,math[,c(inputs,bout)],model="ksvm") # fit a support vector machine
print(B4@object)

# save one model to a file:
print("save B3 to file")
savemodel(B3,"mlpe-pass.model") # saves to file
print("load from file into B5")
B5=loadmodel("mlpe-pass.model") # load from file
print(class(B5@object$mlp[[1]]))
