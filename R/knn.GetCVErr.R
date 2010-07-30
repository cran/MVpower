`knn.GetCVErr` <-
function(.x, statevec,CVlist)
{
NumCVFold <- nrow(CVlist)
NumStates <- length(unique(statevec))

#cv.errors <- matrix(NA, NumCVFold,ncol(.x))
#sample.errors <- matrix(NA, NumCVFold, nrow(.x))

sample.errors.bycvfold <- array(data=NA, dim=c(ncol(.x),NumCVFold, nrow(.x)))


NumStates <- length(unique(statevec))
UStates <- unique(statevec)

    cat("Number of CV fold to be run", NumCVFold, "\n")

 for (j in 1:NumCVFold)
{
    print(j)
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
mydata.in  <- data.frame(y.in, x.in)
        RF1 <- randomForest(x =x.in, y = y.in, importance = TRUE, outscale = TRUE, mtry = mymtry, ntree = 500)
#SVM1 <- ksvm(y.in ~ ., data=mydata.in, type="C-bsvc", kernel="rbfdot", kpar="automatic", cross=4,prob.model=TRUE)
 KNN1 <- knn(train=x.in, test=x.out,cl=y.in,k=5)

sample.errors.bycvfold[NumFtsI,j,indexout.j] <- as.character(KNN1)

if (NumFtsI <= 2){NumFtsI<-1}

if (NumFtsI > 2 & NumFtsI <=100){
   imp <- RF1$importance
   imp.dec.accuracy <- imp[,NumStates+1]
   vars.order <- sort(imp.dec.accuracy,index.return=TRUE)$ix
   vars.keep <- vars.order[2:length(vars.order)]

  x.in <- x.in[,vars.keep]
  x.out <- x.out[,vars.keep]
   NumFtsI <- ncol(x.in)
}

if (NumFtsI > 100){
   imp <- RF1$importance
   imp.dec.accuracy <- imp[,NumStates+1]
  vars.order <- sort(imp.dec.accuracy,index.return=TRUE)$ix
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

ErrorRates <- array(data=NA, dim=c(ncol(.x), NumCVFold, length(unique(statevec)) + 1))

for (i in 1:ncol(.x)){#print(i)
tempi <- sample.errors.bycvfold[i,,]
results.i <- GetCVOutput(tempi, statevec)
ErrorRates[i,,] <- t(results.i$CVErrRates)

}

        out <- apply(t(ErrorRates[,,1]),2,mean,na.rm=TRUE)
#out <- list(ErrorRates=ErrorRates)
return(out)
}

