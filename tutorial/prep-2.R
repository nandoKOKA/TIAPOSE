### row and column selection examples:
# select only virginica data rows:
print("-- 1st example: selection of virginica rows --")
iris2=iris[iris$Species=="virginica",]
cat("iris2",nelems(iris2),"\n")
print(table(iris2$Species))

# select a random subsample of white wine dataset:
print("-- 2nd example: selection of 500 wine samples --")
set.seed(12345) # set for replicability
s=sample(1:nrow(wine),500) # random with 500 indexes
wine2=wine[s,] # wine2 has only 500 samples from wine
cat("wine2",nelems(wine2),"\n")

# column (attribute) selection example:
print("-- 3rd example: selection of 5 wine columns --")
att=c(3,6,8,11,12) # select 5 attributes
wine3=wine2[,att]
cat("wine3",nelems(wine3),"\n")
print(names(wine3))

### attribute transformation examples:
# numeric to discrete: 
# three classes poor=[1 to 4], average=[5 to 6], good=[6 to 10]
print("-- 4th example: numeric to discrete transform (wine) --")
wine3$quality=cut(wine3$quality,c(1,4,6,10),c("poor","average","good"))
print(table(wine2$quality))
print(table(wine3$quality))
# save new data.frame to csv file:
write.table(wine3,file="wine3.csv",row.names=FALSE,col.names=TRUE)

# numeric to numeric:
# log transform to forest fires area (positive skew)
print("-- 5th example: numeric to numeric transform (forest fires) --")
logarea=log(fires$area+1)
pdf("prep2-1.pdf") # create pdf file
par(mfrow=c(2,1))
hist(fires$area,col="gray")
hist(logarea,col="gray")
dev.off() # end of pdf creation

# discrete to discrete (factor to factor)
# transform month into trimesters
print("-- 6th example: discrete to discrete (bank marketing) --")
print(table(bank$month))
tri=delevels(bank$month,levels=c("jan","feb","mar"),label="1st")
tri=delevels(tri,levels=c("apr","may","jun"),label="2nd")
tri=delevels(tri,levels=c("jul","aug","sep"),label="3rd")
tri=delevels(tri,levels=c("oct","nov","dec"),label="4th")
# put the levels in order:
tri=relevel(tri,"1st")
print(table(tri))
# create new data.frame with bank and new atribute:
bank2=cbind(bank[1:9],tri,bank[10:ncol(bank)])
cat("bank2",nelems(bank2),"\n")
print(names(bank2))

# time series to data.frame using a sliding time window
# useful for fitting a data-driven model:
print("-- 6th example: time series to data.frame (internet traffic) --")
dtraffic=CasesSeries(traffic[,2],c(1,2,24),1,30)
print(dtraffic)
