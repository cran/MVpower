`MVpower` <-
function(method, #"PAM", "KNN", "RF", "SVM",
                    delta=1, #effect size mu/sigma
                    NumFts=100, ## total number of features
                    NumImp=5, ## number of markers
                    n1=100, #number of samples
                    n2=100,
                    nfold=4, ## number of CV-folds
                    prior=c(0.5, 0.5), ## prior dist
                    N.null=100, #Number of simulations for null distribution
                    N.pow=100,   #Number of simulations for power calculation
                    null.data=NULL ## if there are already existing null dist, use it instead
                    )
{
    if(! method %in% c("PAM", "KNN", "RF", "SVM"))
        simpleError("Selected method is not PAM, KNN, RF or SVM!")
    ### load all libraries
    if(method == "PAM") library(pamr)
    if(method == "KNN") {library(randomForest);library(kernlab);library(class)}
    if(method == "RF") library(randomForest)
    if(method == "SVM"){library(randomForest);library(kernlab)}

    ###### create null distribution
    if(is.null(null.data)){
        cat("\n Generating null distribution...")
        null.dist <- replicate(N.null, create.dist(method, n1=n1, n2=n2, NumFts=NumFts, nfold=nfold, prior=prior, delta=0, NumImp=NumImp))
        cat("Done!\n")
    }
    if(!is.null(null.data)) null.dist <- as.vector(unlist(null.data))

    ###### create success rate with effect size mu
    cat("\n Generating alternative distribution...")
    alt.dist <- replicate(N.pow, create.dist(method, n1=n1, n2=n2, NumFts=NumFts, nfold=nfold, prior=prior, delta=delta, NumImp=NumImp))
    cat("Done!\n")

    ###### calculate power
    my.prop.rejected <- sum(alt.dist>sort(null.dist)[round(0.95*N.null)])/N.pow

    cat("\n\nClassificaiton Method: ", method,
        "\nNumber of total features", NumFts, ", Number of markers", NumImp,
        "\nNumber of samples: ", n1, ", ", n2,
        "\nEffect size =", delta,
        "\n", nfold, "-fold Cross Validation with priors:",prior,
        "\nNumber of simulations for null distribution:", N.null, ", Number of for alternative:", N.pow, "\n")

    cat("Power=", round(my.prop.rejected*100,1), "% \n\n")
    out <- list(effect.size=delta,
                n1=n1, n2=n2,
                NumFts=NumFts, NumImp=NumImp,
                method=method,
                my.prop.rejected=my.prop.rejected,
                null.dist=null.dist, alt.dist=alt.dist)
    return(out) ##list(my.prop.rejected=my.prop.rejected, my.vec.success=my.vec.success)
}

