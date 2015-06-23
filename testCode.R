
library(caret)
train_dat <- read.csv(file="pml-training.csv")

inTrain <- createDataPartition(y=train_dat$classe, p=0.7, list=F)

training <- train_dat[inTrain,]
crossVal <- train_dat[-inTrain,]

##removing columns with near zero variablity variable
selCol <- nearZeroVar(training, saveMetrics=T)
selCol <- selCol[selCol$nzv==F,] 
selCol_training <- training[,row.names(selCol)]

##removing column with NA value greater than 95%
selCol_training <- selCol_training[, colMeans(is.na(selCol_training)) <= 0.95]

##removing username and timestamp column
selCol_training <- selCol_training[,c(-1,-2,-3,-4,-5)]

ptm <- proc.time()
modFit <- train(classe~., data=selCol_training, method="rf", importance=T)
ptm <- proc.time()
ptm


accuracy <- confusionMatrix(crossVal$classe, predict(modFit, crossVal))
accuracy

final_test_dat <- read.csv(file="pml-testing.csv")
answer = predict(modFit, final_test_dat)
answer

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}