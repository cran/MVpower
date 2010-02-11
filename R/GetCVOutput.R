`GetCVOutput` <-
function(mymat, statevec){
CVErrRates <- apply(mymat, 1, ClassErrFun, statevec)

out <- list(CVErrRates=CVErrRates)
return(out)}

