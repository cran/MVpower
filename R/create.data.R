`create.data` <-
function(n1, n2, NumFts, NumImp, delta, NumRep)
{
    my.data.all <- NULL
    for(i in 1:NumRep)
    {
        print(i)
        my.data <- matrix(rnorm((n1+n2)*NumFts,sd=1),ncol=NumFts)
        my.data[(n1+1):(n1+n2),1:NumImp] <- my.data[(n1+1):(n1+n2),1:NumImp] + delta
        my.data.all[[i]] <- my.data
    }
    return(my.data=my.data.all)
}

