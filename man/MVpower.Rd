\name{MVpower}
\alias{MVpower}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate power for a given effect size and sample size using multivariate
  classification methods }
\description{
  Calculate power for a given effect size and sample size using multivariate
  classification methods: Random forest (RF), Prediction analysis of
  microarrays (PAM), K-nearest neighbor (KNN), or Support vector machine (SVM)
}
\usage{
MVpower(method, delta = 1, NumFts = 100, NumImp = 5, 
n1 = 100, n2 = 100, nfold = 4, prior = c(0.5, 0.5), 
N.null = 100, N.pow = 100, null.data = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{method}{a character string of multivariate classification method
  used, \code{"RF"}, , \code{"PAM"}, \code{"KNN"}, \code{"SVM"}}
  \item{delta}{ numeric value, effect size}
  \item{NumFts}{ number of total features used for prediction}
  \item{NumImp}{ number of total markers}
  \item{n1}{ number of samples in group 1}
  \item{n2}{ number of samples in group 2}
  \item{nfold}{ number of cross validation folds to be run, default is \code{4}}
  \item{prior}{ numeric vector of length 2, prior distribution for the
    two groups, default is \code{c(0.5, 0.5)}}
  \item{N.null}{ number of simulations to get null distribution, default
  is 100}
  \item{N.pow}{ number of simulations to get alternative distribution, default
  is 100}
  \item{null.data}{ default is \code{NULL}, if \code{NULL}, run
    \code{N.null} simulation to generate null distribution; or a numeric
    vector specifying null distribution generated from previous runs}
}
\details{

  The classifiers are evaluated in the setting in which the number of features measured exceeds the number of subjects in the study. 

  Power for the four listed classifiers is calculated by the following
  simulation that incorporates a recursive feature elimination procedure
  to discover an optimal subset of features that resulted in maximum
  success of the estimated classifier in predicting each subject's class
  membership.

  For each simulated dataset, a p value associated with the classification metric (classification accuracy) was calculated based on comparison of the observed value of the statistic to its distribution under the null, obtained when all features were non-discriminating between cases and controls. Statistical power was estimated as the proportion of simulated datasets resulting in classifiers with associated p values less than or equal to 0.05. The results reported in the paper are averages based on N.pow simulated datasets. 

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
\author{ Raji Balasubramanian and Yu Guo }
%\note{ ~~further notes~~ 

%}
%\section{Warning }{....} 
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{

pam1 <- MVpower("PAM",
                    delta=0.4, #effect size mu/sigma
                    NumFts=1000, ## total number of features
                    NumImp=10, ## number of markers
                    n1=100, #number of samples
                    n2=100,
                    nfold=4, ## number of CV-folds
                    prior=c(0.5, 0.5), ## prior dist
                    N.null=5, #Number of simulations for null distribution
                    N.pow=5   #Number of simulations for power calculation
                    )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ Statistics|tree }
%\keyword{ Statistics|multivariate }% __ONLY ONE__ keyword per line
\keyword{ tree }
\keyword{ multivariate }