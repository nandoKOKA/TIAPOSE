# simple show rows x columns function
nelems=function(d) paste(nrow(d),"x",ncol(d))

# load the famous Iris dataset:
data(iris) # load the data
cat("iris:",nelems(iris),"\n")
print(class(iris)) # show class
print(names(iris)) # show attributes

### load all my UCI ML datasets ###

# White Wine Quality dataset:
URL="http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
wine=read.table(file=URL,header=TRUE,sep=";")
cat("wine quality white:",nelems(wine),"\n")
print(class(wine)) # show class
nelems(wine) # show rows x columns
print(names(wine)) # show attributes

# forest fires dataset:
URL="http://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv"
fires=read.table(file=URL,header=TRUE,sep=",") 
cat("forest fires:",nelems(fires),"\n")
print(class(fires)) # show class
print(names(fires)) # show attributes

# load bank marketing dataset (in zip file):
URL="http://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip"
temp=tempfile() # temporary file
download.file(URL,temp) # download file to temporary
# unzip file and load into data.frame:
bank=read.table(unz(temp,"bank-additional/bank-additional.csv"),sep=";",header=TRUE)
cat("bank marketing:",nelems(bank),"\n")
print(class(bank)) # show class
print(names(bank)) # show attributes

# student performance dataset (in zip file):
URL="http://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip"
temp=tempfile() # temporary file
download.file(URL,temp) # download file to temporary
# unzip file and load into data.frame:
math=read.table(unz(temp,"student-mat.csv"),sep=";",header=TRUE)
cat("student performance math:",nelems(math),"\n")
print(class(math)) # show class
print(names(math)) # show attributes
# save data.frame to csv file:
write.table(math,file="math.csv",row.names=FALSE,col.names=TRUE)

# internet traffic (time series):
URL="http://www3.dsi.uminho.pt/pcortez/data/internet-traffic-data-in-bits-fr.csv"
traffic=read.table(URL,sep=";",header=TRUE)
cat("internet traffic:",nelems(traffic),"\n")
print(class(traffic)) # show class
print(names(traffic)) # show attributes

