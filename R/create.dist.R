`create.dist` <-
function(method, n1,n2, NumFts, nfold, prior, delta, NumImp)
{
    my.data <- matrix(rnorm((n1+n2)*NumFts,sd=1),ncol=NumFts)
    my.groups <- factor(c(rep(1,n1),rep(2,n2)))
    my.data[(n1+1):(n1+n2),1:NumImp] <- my.data[(n1+1):(n1+n2),1:NumImp] + delta

    ### Create cross validation matrix
    CVlist.r <- c()
    for (i in 1:nfold)
        CVlist.r <- rbind(CVlist.r, GetCVList(c(1/nfold,1), my.groups))

        ### Obtain external cross validation based error
        if(method == "KNN")
            average.errors <- knn.GetCVErr(my.data, 1:NumFts, my.groups, CVlist.r)
        if(method == "RF")
            average.errors <- rf.GetCVErr(my.data, 1:NumFts, my.groups, numtree=500, CVlist.r, MyClassWt=prior, MySampSize=NULL)
        if(method == "SVM")
            average.errors <- svm.GetCVErr(my.data, 1:NumFts, my.groups, CVlist.r)
        if(method == "PAM")
            average.errors <- pam.GetCVErr(my.data, my.groups, nfold, prior)

    return(2*(1-min(average.errors, na.rm=T)))
}

