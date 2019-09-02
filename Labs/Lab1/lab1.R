library(cluster)
library(lars)
library(glmnet)
library(leaps)
library(Matrix)
library(RandPro)

raw <- read.csv("C:/Users/Konstantinos/Desktop/Tilburg University/Decision Making with Business Analytics/Homeworks/Labs/Lab1/unempstatesChanges.csv")

rawt=matrix(nrow=50,ncol=416)
rawt=t(raw)

## k-means clustering in 415 dimensions
set.seed(1)
grpunemp <- kmeans(rawt, centers=2, nstart=10)
sort(grpunemp$cluster)

## load data set unemp.csv
unemp <- read.csv("C:/Users/Konstantinos/Desktop/Tilburg University/Decision Making with Business Analytics/Homeworks/Labs/Lab1/unemp.csv")

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

for (i in c(1:4)){
  if (i==1){
    xfinal1=xVars[[i]]
  }else{
    xfinal1=cbind(xfinal1, xVars[[i]])
  }
}

names = list()
j = 1
for (i in c(1:16)) {
  name = paste0("X", i)
  names[[i]] <- name 
  
}
colnames(xfinal1)<-names

## Question v
lasso_predictions=list()
for (i in c(1:4)){
  lasso1=cv.glmnet(xFinal, yTables[[i]], alpha=1, lambda=c(0.01, 0.1, 1, 10), thresh=1e-12)
  lasso.pred = predict(lasso1, newx=xFinal, s=c(0.01, 0.1, 1, 10))
  lasso_predictions[[i]]=lasso.pred
}

## Question vi

total_no_cols = c(1:200)
sample_indices = sample(total_no_cols,20)

new_dataset = data.frame(xFinal)

new_dataset <- new_dataset[sample_indices]

rand_projt=list()

for (i in c(1:4)){
  rand_proj = lm(yTables[[i]]~., data=data.frame(new_dataset))
  rand_projt[[i]]=rand_proj
}

## Questions 1-6
final_dataset = xFinal
for (i in c(1:4)) {
  final_dataset = data.frame(cbind(final_dataset, yTables[[i]]))
}

colnames(final_dataset)[ncol(final_dataset) - 3] <- "Y1"
colnames(final_dataset)[ncol(final_dataset) - 2] <- "Y2"
colnames(final_dataset)[ncol(final_dataset) - 1] <- "Y3"
colnames(final_dataset)[ncol(final_dataset)] <- "Y4"

train_df = data.frame(final_dataset[1:362,])
test_df = data.frame(final_dataset[362:411,])

#linear regression
lm_list=list()
lasso_list=list()
for (i in c(1:4)){
  lm.fit=lm(yTables[[i]]~., data=train_df)
  listcoeflm[[i]]=lm.fit
  lm.pred=predict(lm.fit, test_df)
  lm_list[[i]]=lm.pred
  
  #lasso
  train.mat = model.matrix(yTables[[i]]~., data=train_df)
  test.mat = model.matrix(yTables[[i]]~., data=test_df)
  mod.lasso = cv.glmnet(train.mat, train_df[, yTables[[i]]], alpha=1, lambda=grid, thresh=1e-12)
  lambda.best = mod.lasso$lambda.min
  lasso.pred = predict(mod.lasso, newx=test.mat, s=lambda.best)
  lasso_list[[i]]=lasso.pred
}



#lasso
train.mat = model.matrix(Wage~., data=traindf)
test.mat = model.matrix(Wage~., data=testdf)
mod.lasso = cv.glmnet(train.mat, traindf[, "Wage"], alpha=1, lambda=grid, thresh=1e-12)
lambda.best = mod.lasso$lambda.min
lasso.pred = predict(mod.lasso, newx=test.mat, s=lambda.best)
testerror.lasso=mean((testdf[, "Wage"] - lasso.pred)^2)

mod.lasso = glmnet(model.matrix(Wage~., data=df), df[, "Wage"], alpha=1)
coef.lasso=predict(mod.lasso, s=lambda.best, type="coefficients")
listcoeflasso[[i]]=coef.lasso
length(which(coef.lasso[-(1:2),]!=0))
listerrorlasso[[i]]=testerror.lasso











