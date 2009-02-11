\name{BCPNN}
\alias{BCPNN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bayesian confidence propagation neural network}
\description{
Bayesian confidence propagation neural network (Bate et al. 1998, Noren et al. 2006) extended to the multiple comparison framework. 
}
\usage{
BCPNN(DATABASE, RR0 = 1, MIN.n11 = 1, DECISION = 1, DECISION.THRES = 0.05, 
RANKSTAT = 1, MC = FALSE, NB.MC = 10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DATABASE}{Object returned by the function \code{as.PhViD}.}
  \item{RR0}{Value of the tested risk. By default, \code{RR0=1}.}
  \item{MIN.n11}{Minimum number of notifications for a couple to be potentially considered as a signal. By default, \code{MIN.n11 = 1}.}
  \item{DECISION}{Decision rule for the signal generation based on
 
                 1 = FDR (Default value)

                 2 = Number of signals 

                 3 = Ranking statistic. See \code{RANKSTAT}}
  \item{DECISION.THRES}{Threshold for \code{DECISION}. Ex 0.05 for FDR (\code{DECISION}=1). }
  \item{RANKSTAT}{Statistic used for ranking the couples:

                 1 = Posterior probability of the null hypothesis

                 2 = 2.5\% quantile of the posterior distribution of IC.}
  \item{MC}{If \code{MC=TRUE}, the statistic of interest (see \code{RANKSTAT}) is calculated by Monte Carlo simulations which can be very long. If \code{MC=FALSE}, IC is approximated by a normal distribution (which can be very crude for small counts).}
  \item{NB.MC}{If \code{MC=TRUE}, \code{NB.MC} indicates the number of Monte Carlo simulations to be done}
}
\details{
  The BCPNN method is based on the calculation of the Information Component IC. If \code{MC = FALSE}, the bayesian model used is the beta-binomial proposed by Bate et al. (1998). The statistic of interest (see \code{RANKSTAT}) is calculated by the normal approximation made in Bate et al. (1998) with the use of the exact expectation and variance proposed by Gould (2003). If \code{MC = TRUE}, the model is based on the Dirichlet-multinomial model proposed more recently in Noren et al. (2006). In this case, the statistic of interest is calculated by Monte Carlo simulations. 
}
\value{
  \item{ALLSIGNALS}{Data.frame summarizing the results of all couples with at least \code{MIN.n11} notifications ordered by \code{RANKSTAT}. It contains notably the labels, the cell counts, the expected counts (\eqn{n1. \times n.1 / N}{n1. * n.1 / N}, see \code{\link{as.PhViD}}), \code{RANKSTAT}, the ratios(count/expected count), the marginal counts and the estimations of FDR, FNR, Se et Sp. If \code{RANKSTAT!=1}, the last column is the posterior probability of the null hypothesis.}
  \item{SIGNALS}{Same Data.frame as \code{ALLSIGNALS} but restricted to the list of generated signals.}
  \item{NB.SIGNALS}{Number of generated signals.} 
  \item{INPUT.PARAM}{Parameters entered in the function.}
}
\references{
Ahmed I, Haramburu F, Fourrier-Reglat A, Thiessard F,  Kreft-Jais C, Miremont-Salame G, Begaud B, Tubert-Bitter P. Bayesian Pharmacovigilance Signal Detection Methods Revisited in a Multiple Comparison Setting, Statistics in Medicine, accepted.

Bate A, Lindquist M, Edwards IR, Olsson S, Orre R, Lansner A, De Freitas RM, A Bayesian Neural Network Method for Adverse Drug Reaction Signal Generation European Journal of Clinical Pharmacology, 1998, 54, 315-321.

Gould AL, Practical Pharmacovigilance Analysis Strategies Pharmacoepidemiology and Drug Safety, 2003, 12, 559-574

Noren, GN, Bate A, Orre R, Edwards IR, Extending the methods used to screen the WHO drug safety database towards analysis of complex associations and improved accuracy for rare events Statistics in Medicine, 2006, 25, 3740-3757.
}
\examples{
## start
data(PhViDdata.frame)
PhViDdata <- as.PhViD(PhViDdata.frame)
res <- BCPNN(PhViDdata)
## end
}
\author{Ismail Ahmed & Antoine Poncet}
\keyword{ htest }
