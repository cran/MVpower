`pam.GetCVErr` <-
function(my.data, my.groups, nfold, prior)
{
    pam.fit <- pamr.train(data=list(x=t(my.data),y=my.groups),prior=prior)
    pam.cv <- pamr.cv(pam.fit,data=list(x=t(my.data),y=my.groups),nfold=nfold)
   return(pam.cv$error)
}

