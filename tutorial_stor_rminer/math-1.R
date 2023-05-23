# preparation
math=read.table(file="math.csv",header=TRUE) # read previously saved file

# binary task:
pass=cut(math$G3,c(-1,9,20),c("fail","pass"))
# five-level system:
five=cut(math$G3,c(-1,9,11,13,15,20),c("F","D","C","B","A")) # Ireland grades
# create pdf:
pdf("math-grades.pdf")
par(mfrow=c(1,3))
plot(pass,main="pass")
plot(five,main="five")
hist(math$G3,col="gray",main="G3",xlab="")
dev.off() # end of pdf creation

# creating the full dataset:
d=cbind(math,pass,five)
write.table(d,"math2.csv",row.names=FALSE,col.names=TRUE) # save to file
