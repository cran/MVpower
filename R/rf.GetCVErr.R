`rf.GetCVErr` <-
function(.x, .fts, statevec, numtree,CVlist,MyClassWt,MySampSize)
{
NumCVFold <- nrow(CVlist)
NumStates <- length(unique(statevec))

#cv.errors <- matrix(NA, NumCVFold,ncol(.x))
#sample.errors <- matrix(NA, NumCVFold, nrow(.x))

sample.errors.bycvfold <- array(data=NA, dim=c(ncol(.x),NumCVFold, nrow(.x)))
sample.votes.bycvfold  <- array(data=NA, dim=c(ncol(.x), NumCVFold, nrow(.x),NumStates))

NumStates <- length(unique(statevec))
UStates <- unique(statevec)
for (j in 1:NumCVFold){#print(j)
indexout.j <- CVlist[j,]
indexout.j <- indexout.j[!is.na(indexout.j)]

### Defining the training and test data for the j-th CV step

x.in<- .x[-indexout.j,]
y.in <- statevec[-indexout.j]
x.out <- .x[indexout.j,]
y.out <- statevec[indexout.j]

NumFtsI <- ncol(x.in)
while(NumFtsI >= 2)
{
mymtry <- round(NumFtsI^0.5)
if(!is.null(MySampSize)){
        RF1 <- randomForest(x =x.in, y = y.in, importance = TRUE, outscale = TRUE, mtry = mymtry, ntree = numtree,classwt = MyClassWt, sampsize=MySampSize)}
if(is.null(MySampSize)){
        RF1 <- randomForest(x =x.in, y = y.in, importance = TRUE, outscale = TRUE, mtry = mymtry, ntree = numtree,classwt = MyClassWt)}

#num <- nrow(x.out)
#cv.errors[j,NumFtsI] <- (num - sum(y.out == predict(RF1,x.out)))/num

sample.errors.bycvfold[NumFtsI,j,indexout.j] <- as.character(predict(RF1, x.out))
sample.votes.bycvfold[NumFtsI,j,indexout.j,] <- predict(RF1,x.out,type="prob")

if (NumFtsI <= 2){NumFtsI<-1}

if (NumFtsI > 2 & NumFtsI <=100){
   imp <- RF1$importance
   imp.dec.accuracy <- imp[,NumStates+1]
   vars.order <- sort(imp.dec.accuracy,index.return=T)$ix
   vars.keep <- vars.order[2:length(vars.order)]

  x.in <- x.in[,vars.keep]
  x.out <- x.out[,vars.keep]
   NumFtsI <- ncol(x.in)
}

if (NumFtsI > 100){
   imp <- RF1$importance
   imp.dec.accuracy <- imp[,NumStates+1]
  vars.order <- sort(imp.dec.accuracy,index.return=T)$ix
   keep.percent <- round(0.1*NumFtsI)
   vars.keep <- vars.order[(keep.percent+1):length(vars.order)]
   x.in <- x.in[,vars.keep]
   x.out <- x.out[,vars.keep]
   NumFtsI <- ncol(x.in)
}
}
}


#### Output an average CV error (overall classes and class specific) as a function of number of features in classifier
#### Output a predicted state for each subject as a function of the number of features in classifier


SamplePredictions <- matrix(NA, nrow(.x), ncol(.x))
SamplePrediction.ClassProb <- matrix(NA, nrow(.x), ncol(.x))
ErrorRates <- array(data=NA, dim=c(ncol(.x), NumCVFold, length(unique(statevec)) + 1))
ErrorRatesFromSamplePred <- array(data=NA, dim=c(ncol(.x), length(unique(statevec)) + 1))

for (i in 1:ncol(.x)){#print(i)
tempi <- sample.errors.bycvfold[i,,]
results.i <- GetCVOutput(tempi, statevec)
#SamplePredictions[,i] <- results.i$SamplePreds
#SamplePrediction.ClassProb[,i] <- results.i$SamplePredErrs
ErrorRates[i,,] <- t(results.i$CVErrRates)
#ErrorRatesFromSamplePred[i,] <- ClassErrFun(results.i$SamplePreds, statevec)

}

#out <- list(sample.votes.bycvfold  = sample.votes.bycvfold, sample.errors.bycvfold  = sample.errors.bycvfold, SamplePredictions=SamplePredictions,SamplePrediction.ClassProb=SamplePrediction.ClassProb,ErrorRates=ErrorRates,ErrorRatesFromSamplePred=ErrorRatesFromSamplePred)

        out <- apply(t(ErrorRates[,,1]),2,mean,na.rm=T)
return(out)
}

