`create.dist` <-
function(method, nfold, prior, my.data, my.groups)
{
    ### Create cross validation matrix
    CVlist.r <- c()
    for (i in 1:nfold)
        CVlist.r <- rbind(CVlist.r, GetCVList(c(1/nfold,1), my.groups))

    ### Obtain external cross validation based error
    if(method == "KNN")
        average.errors <- knn.GetCVErr(.x=my.data, statevec=my.groups, CVlist=CVlist.r)
    if(method == "RF")
        average.errors <- rf.GetCVErr(.x=my.data, statevec=my.groups, numtree=500, CVlist=CVlist.r, MyClassWt=prior, MySampSize=NULL)

    if(method == "SVM")
        average.errors <- svm.GetCVErr(.x=my.data, statevec=my.groups, CVlist=CVlist.r)
    if(method == "PAM")
        average.errors <- pam.GetCVErr(my.data=my.data, my.groups=my.groups, nfold=nfold, prior=prior)

    return(2*(1-min(average.errors, na.rm=TRUE)))
}

