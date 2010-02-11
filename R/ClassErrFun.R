`ClassErrFun` <-
function(myvec, statevec)
{
    NumClasses <- length(unique(statevec))
    ustates <- sort(unique(statevec))

    if (sum(!is.na(myvec)) > 0){
        overall.error <- sum(!is.na(myvec) & as.character(statevec) != as.character(myvec))/sum(!is.na(myvec))}
    if (sum(!is.na(myvec)) == 0){overall.error <- NA}

    class.errors <- c()
    for (i in 1:NumClasses){
        ind <- statevec == ustates[i]
        myveci <- myvec[ind]
        stateveci <- statevec[ind]
        if (sum(!is.na(myveci)) > 0){
            ce.i <- sum(!is.na(myveci) & as.character(stateveci) != as.character(myveci))/sum(!is.na(myveci))}
        if (sum(!is.na(myveci)) == 0){ce.i <- NA}
        class.errors <- c(class.errors, ce.i)}

    out <- c(overall.error, class.errors)
    return(out)
}

