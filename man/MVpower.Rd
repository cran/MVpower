\name{MVpower}
\alias{MVpower}
\alias{MVpower-package}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate power for a given effect size and sample size using multivariate
  classification methods }

\description{
   Calculate power for a given effect size and sample size using multivariate
  classification methods: Random forest (RF), Prediction analysis of
  microarrays (PAM), K-nearest neighbor (KNN), or Support vector machine (SVM)
}
\usage{
MVpower(method, prior = c(0.5, 0.5), nfold = 4, delta = 1, NumFts = 100, NumImp = 5, n1 = 100, n2 = 100, N.null = 100, N.pow = 100, alt.data = NULL, null.data = NULL, my.groups = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{method}{a character string of multivariate classification method
    used, \code{"RF"}, , \code{"PAM"}, \code{"KNN"}, \code{"SVM"}}
  \item{prior}{numeric vector of length 2, prior distribution for the
    two groups, default is \code{c(0.5, 0.5)}}
  \item{nfold}{number of cross validation folds to be run, default is \code{4}}
  \item{delta}{ numeric value, effect size. Used only if user provided
    alternative data (\code{alt.data}) is not provided}
  \item{NumFts}{ number of total features used for prediction. Used only if user provided alternative data (\code{alt.data}) is not provided}
  \item{NumImp}{ number of total markers. Used only if user provided
    alternative data (\code{alt.data}) is not provided}
  \item{n1}{ number of samples in group 1. Used only if user provided
    alternative data (\code{alt.data}) is not provided}
  \item{n2}{ number of samples in group 2. Used only if user provided
    alternative data (\code{alt.data}) is not provided}
  \item{N.null}{ number of simulations to get null distribution, default
    is 100. Used only if user provided null data (\code{null.data}) is not provided}
  \item{N.pow}{ number of simulations to get alternative distribution, default
    is 100. Used only if user provided alternative data (\code{alt.data}) is not provided}
  \item{alt.data}{User generated alternative distribution. Default is \code{NULL}, if \code{NULL}, run
    \code{N.pow} simulation to generate alternative distribution; or a
    list specifying alternative distribution the user generated, each element is
    a data matrix with rows as samples and columns as features.}
  \item{null.data}{User generated null distribution. Default is \code{NULL}, if \code{NULL}, run
    \code{N.null} simulation to generate null distribution; or a
    list specifying null distribution the user generated, each element is
    a data matrix with rows as samples and columns as features.}
  \item{my.groups}{User provided group indicator. Default is \code{NULL}, if \code{NULL}, it is set to
   be a factor with 2 levels, and \code{n1} and \code{n2} samples in
   each level, individually.}
}
\details{

  The classifiers are evaluated in the setting in which the number of features measured exceeds the number of subjects in the study. 

  Power for the four listed classifiers is calculated by the following
  simulation that incorporates a recursive feature elimination procedure
  to discover an optimal subset of features that resulted in maximum
  success of the estimated classifier in predicting each subject's class
  membership.

  For each simulated dataset, a p value associated with the classification metric (classification accuracy) was calculated based on comparison of the observed value of the statistic to its distribution under the null, obtained when all features were non-discriminating between cases and controls. Statistical power was estimated as the proportion of simulated datasets resulting in classifiers with associated p values less than or equal to 0.05. The results reported in the paper are averages based on \code{N.pow} simulated datasets. 

}
\value{
 a list of the following components
  \item{effect.size}{effect size}
  \item{n1}{number of samples in group 1}
  \item{n2}{number of samples in group 2}
  \item{NumFts}{number of total features used for prediction}
  \item{NumImp}{ number of total markers}
  \item{method}{multivariate classification method used}
  \item{my.prop.rejected}{power}
  \item{null.dist}{Null distribution}
  \item{alt.dist}{Alternative distribution}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Yu Guo and Raji Balasubramanian}
%\note{ ~~further notes~~ 
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{

#an exmaple using MVpower function to generate null and alternative distribution
MVpower(method="PAM", #"PAM", "KNN", "RF", "SVM",
        prior=c(0.5, 0.5), ## prior dist
        nfold=4, ## number of CV-folds
        delta=.5, #effect size mu/sigma
        NumFts=100, ## total number of features
        NumImp=5, ## number of markers
        n1=100, #number of samples
        n2=100,
        N.null=10, #Number of simulations for null distribution
        N.pow=10,   #Number of simulations for power calculation
        alt.data=NULL, null.data=NULL, my.groups=NULL)

# an example using user generated null and alternative distribution
# simulate normally distributed data with 10 samples in each group,
# 30 analytes, 5 markers, effect.size=1
null.data <- alt.data <- NULL
for(i in 1:10)
    {
        my.data <- matrix(rnorm(20*30,sd=1),ncol=30)
        my.data[11:20,1:5] <- my.data[11:20,1:5]
        null.data[[i]] <- my.data
    }
for(i in 1:10)
    {
        my.data <- matrix(rnorm(20*30,sd=1),ncol=30)
        my.data[11:20,1:5] <- my.data[11:20,1:5] + 1
        alt.data[[i]] <- my.data
    }
my.groups <- factor(rep(1:2,each=10))

MVpower(method="PAM", #"PAM", "KNN", "RF", "SVM",
        prior=c(0.5, 0.5), ## prior dist
        nfold=4, ## number of CV-folds
        delta=NULL, NumFts=NULL, NumImp=NULL, n1=NULL, n2=NULL, N.null=NULL, N.pow=NULL,  
        alt.data=alt.data,
        null.data=null.data,
        my.groups=my.groups)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ tree }
\keyword{ multivariate }
