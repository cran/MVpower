`MVpower` <-
function(method, #"PAM", "KNN", "RF", "SVM",
         prior=c(0.5, 0.5), ## prior dist
         nfold=4, ## number of CV-folds

                    delta=1, #effect size mu/sigma
                    NumFts=100, ## total number of features
                    NumImp=5, ## number of markers
                    n1=100, #number of samples
                    n2=100,
                    N.null=100, #Number of simulations for null distribution
                    N.pow=100,   #Number of simulations for power calculation

                    alt.data=NULL,## user provided null and alternative distribution data, a list with N.pow compoments, each with my.data, and my.groups
                    null.data=NULL,
                    my.groups=NULL ## a factor with 2 levels indicating group
                    )
{
    now <- proc.time()

    if(! method %in% c("PAM", "KNN", "RF", "SVM"))
        simpleError("Selected method is not PAM, KNN, RF or SVM!")
    ### load all libraries
    if(method == "PAM") library(pamr)
    if(method == "KNN") {library(randomForest);library(kernlab);library(class)}
    if(method == "RF") library(randomForest)
    if(method == "SVM"){library(randomForest);library(kernlab)}

    cat("Method chosen:", method, "\n")

    if(is.null(my.groups)) my.groups <- factor(c(rep(1,n1),rep(2,n2)))

    ###### create null distribution
    if(is.null(null.data)){ ## if no null.data is provided, then create a null distribution
        cat("\n Generating null distribution...")
        null.data <- create.data(n1, n2, NumFts, NumImp, delta=0, NumRep=N.null)
        cat("Null distribution generated!\n")
    }else ## if null.data is provided, then calculate the average error
    cat("\n Use user provided null distribution...")

    ####### create alternative distribution
    if(is.null(alt.data)){ ## if no alt.data is provided, then create an alternative distribution
        cat("\n Generating alternative distribution...\n")
        alt.data <- create.data(n1, n2, NumFts, NumImp, delta, NumRep=N.pow)
        cat("Alternative distribution generated!\n")
    }else ## if null.data is provided, then calculate the average error
    cat("\n Use user provided alternative distribution...\n")

    ############# check dimensions
    if(any(sapply(null.data, nrow, simplify=TRUE)!=length(my.groups)))
        simpleError("Provided group length is different from privided NULL data size!")
    if(any(sapply(alt.data, nrow, simplify=TRUE)!=length(my.groups)))
        simpleError("Provided group length is different from privided Alternative data size!")
    if(any(sapply(alt.data, nrow, simplify=TRUE)!=sapply(null.data, nrow, simplify=TRUE)))
        simpleError("Provided Null data is different from privided Alternative data size!")
    if(is.null(prior)) {prior = c(0.5, 0.5); print("Equal prior is used!")}
    if(is.null(nfold)) simpleError("Please Provide Number of CV fold to be run!")

    cat("\nGenerating null error distribution. Total",length(null.data),"At ")
    null.dist <- NULL
    for(i in 1:length(null.data))
    {   cat(i,"")
        null.dist[i] <- create.dist(method, nfold=nfold, prior=prior, my.data=null.data[[i]], my.groups=my.groups)
    }
    cat("\nNull error distribution generated.\n")

    cat("\nGenerating alternative error distribution. Total",length(alt.data),"At ")
    alt.dist <- NULL
    for(i in 1:length(alt.data))
    {   cat(i,"")
        alt.dist[i] <- create.dist(method, nfold=nfold, prior=prior, my.data=alt.data[[i]], my.groups=my.groups)
    }
    cat("\nAlternative error distribution generated.\n")

    ###### calculate power
    my.prop.rejected <- mean(alt.dist>sort(null.dist)[round(0.95*length(null.dist))])

    cat("\n\nClassificaiton Method: ", method,
        "\nNumber of total features:", ncol(null.data[[1]]),
        ", Number of markers:",  if(!is.null(NumImp)) NumImp else "User Defined",
        "\nNumber of samples: ", table(my.groups)[1], ", ", table(my.groups)[2],
        "\nEffect size =", if(!is.null(delta)) delta else "User Defined",
        "\n", nfold, "-fold Cross Validation with priors:",prior,
        "\nNumber of simulations for null distribution:", length(null.data), ", Number of for alternative:", length(alt.data), "\n")

    cat("Power=", round(my.prop.rejected*100,1), "% \n\n")
    out <- list(effect.size=if(!is.null(delta)) delta else "User Defined",
                n1=table(my.groups)[1], n2=table(my.groups)[2],
                NumFts=ncol(null.data[[1]]),
                NumImp=if(!is.null(NumImp)) NumImp else "User Defined",
                method=method,
                my.prop.rejected=my.prop.rejected,
                null.dist=null.dist, alt.dist=alt.dist)

    cat("Running time:", (now-proc.time())[3], "\n")
    return(out) ##list(my.prop.rejected=my.prop.rejected, my.vec.success=my.vec.success)
}

