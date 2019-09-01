library(cluster)
library(lars)

raw <- read.csv("C:/Users/Konstantinos/Desktop/Tilburg University/Decision Making with Business Analytics/Lab1/unempstatesChanges.csv")

rawt=matrix(nrow=50,ncol=416)
rawt=t(raw)

## k-means clustering in 415 dimensions
set.seed(1)
grpunemp <- kmeans(rawt, centers=2, nstart=10)
sort(grpunemp$cluster)

## load data set unemp.csv
unemp <- read.csv("C:/Users/Konstantinos/Desktop/Tilburg University/Decision Making with Business Analytics/Lab1/unemp.csv")

## list of cluster assignments
o=order(grpunemp$cluster)

## plot clusters
data.frame(unemp$state[o],grpunemp$cluster[o])
plot(unemp$mean,unemp$stddev,type="n",xlab="mean", ylab="stddev")
text(x=unemp$mean,y=unemp$stddev,labels=unemp$state, col=grpunemp$cluster+1)

i=3
## 3 clusters
for (i in c(3:5)){
  grpunemp <- kmeans(rawt, centers=i, nstart=10)
  sort(grpunemp$cluster)
  o=order(grpunemp$cluster)
  data.frame(unemp$state[o],grpunemp$cluster[o])
  plot(unemp$mean,unemp$stddev,type="n",xlab="mean", ylab="stddev")
  text(x=unemp$mean,y=unemp$stddev,labels=unemp$state, col=grpunemp$cluster+1)
}

grpunemp <- kmeans(rawt, centers=4, nstart=10)
sort(grpunemp$cluster)
o=order(grpunemp$cluster)
data.frame(unemp$state[o],grpunemp$cluster[o])
plot(unemp$mean,unemp$stddev,type="n",xlab="mean", ylab="stddev")
text(x=unemp$mean,y=unemp$stddev,labels=unemp$state, col=grpunemp$cluster+1)


which(grpunemp$cluster==1)
indices=list(1,2,18,24)


av1=mean(rawt[1,412:415])
av2=mean(rawt[2,412:415])
av3=mean(rawt[18,412:415])
av4=mean(rawt[24,412:415])

xVars = list()
yTables = list()

for (i in c(1:4)){
  index = indices[[i]]
  xTable  = data.frame(matrix(nrow=411,ncol=4))
  yTable = c(rawt[index, 5:415])
  yTables[[i]] <- yTable
  
  for (j in c(1:411)) {
      xTable = rbind(xTable, rawt[index, j:j+3])
  }
  xTable = na.omit(xTable)
  xVars[[i]] <- data.matrix(xTable)
}

## question ii
trainings = list()
for (i in c(1:4)) {
  combined = cbind(xVars[[i]], yTables[[i]])
  training = lm(yTables[[i]]~xVars[[i]], data=data.frame(combined))
  trainings[[i]]<-training
}

## Question iii

xList = list()

# create list of tables. each table is associated with a state
for (i in c(1:50)) {
  tempTable = data.frame(matrix(nrow=411,ncol=4))
  for (j in c(1:411)) {
    tempTable = rbind(tempTable, rawt[i, j:j+3])
  }
  tempTable = na.omit(tempTable)
  xList[[i]] <- data.matrix(tempTable)
}

# combine all the tables in a single one
for (i in c(1:50)) {
  tempTable = xList[[i]]
  if (i ==1) {
    ## arxikopoiisi xFinal me to table ths 1hs politeias
    xFinal = tempTable
  } else {
    ## cbind me ta tables twn upoloipwn politeiwn
    xFinal = cbind(xFinal, tempTable)
  }
}

# change variable names
names = list()
j = 1
for (i in c(1:200)) {
    name = paste0("X", i)
    names[[i]] <- name 

}
colnames(xFinal)<-names

# linear regression
trained = lm(yTables[[1]]~., data=data.frame(xFinal))

## Question iv














