# missing data example
# since bank does not include missing data, lets
# synthetically create such data:
set.seed(12345) # set for replicability
bank3=bank
N=500 # randomly assign N missing values (NA) to 1st and 2nd attributes
srow1=sample(1:nrow(bank),N) # N rows
srow2=sample(1:nrow(bank),N) # N rows
bank3[srow1,1]=NA # age
bank3[srow2,2]=NA # job
print("Show summary of bank3 1st and 2nd attributes (with NA values):")
print(summary(bank3[,1:2]))
cat("bank3:",nelems(bank3),"\n")
cat("NA values:",sum(is.na(bank3)),"\n")

# 1st method: case deletion
print("-- 1st method: case deletion --")
bank4=na.omit(bank3)
cat("bank4:",nelems(bank4),"\n")
cat("NA values:",sum(is.na(bank4)),"\n")

# 2nd method: average imputation for age, mode imputation for job:
# substitute NA values by the mean:
print("-- 2nd method: value imputation --")
print("original age summary:")
print(summary(bank3$age))
meanage=mean(bank3$age,na.rm=TRUE)
bank5=imputation("value",bank3,"age",Value=meanage)
print("mean imputation age summary:")
print(summary(bank5$age))
# substitute NA values by the mode (most common value of bank$job):
print("original job summary:")
print(summary(bank3$job))
bank5=imputation("value",bank5,"job",Value=names(which.max(table(bank$job))))
print("mode imputation job summary:")
print(summary(bank5$job))

# 3rd method: hot deck
# substitute NA values by the values found in most similar case (1-nearest neighbor):
print("-- 3rd method: hotdeck imputation --")
print("original age summary:")
print(summary(bank3$age))
bank6=imputation("hotdeck",bank3,"age")
print("hot deck imputation age summary:")
print(summary(bank6$age))
# substitute NA values by the values found in most similar case:
print("original job summary:")
print(summary(bank3$job))
bank6=imputation("hotdeck",bank6,"job")
print("hot deck imputation job summary:")
print(summary(bank6$job))
cat("bank6:",nelems(bank6),"\n")
cat("NA values:",sum(is.na(bank6)),"\n")

# comparison of age densities (mean vs hotdeck):
library(ggplot2)
meth1=data.frame(length=bank4$age)
meth2=data.frame(length=bank5$age)
meth3=data.frame(length=bank6$age)
meth1$method="original"
meth2$method="average"
meth3$method="hotdeck"
all=rbind(meth1,meth2,meth3)
ggplot(all,aes(length,fill=method))+geom_density(alpha = 0.2)
ggsave(file="prep3-1.pdf")
