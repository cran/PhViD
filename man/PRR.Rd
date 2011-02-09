\encoding{UTF-8}
\name{PRR}
\alias{PRR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Proportional Reporting Ratio}
\description{
Proportional Reporting Ratio proposed by Evans et al. (2001) extended to the multiple comparison framework. Note that the computed variance is different from the one used in van Puijenbroek et al. (2002)
}
\usage{
PRR(DATABASE, RR0 = 1, MIN.n11 = 1, DECISION = 1, 
DECISION.THRES = 0.05, RANKSTAT = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DATABASE}{Object returned by the function \code{as.PhViD}.}
  \item{RR0}{Value of the tested relative risk. By default, \code{RR0=1}.}
  \item{MIN.n11}{Minimum number of notifications for a couple to be potentially considered as a signal. By default, \code{MIN.n11 = 1}.}
  \item{DECISION}{Decision rule for the signal generation based on  
                 1 = FDR (Default value) 

                 2 = Number of signals
 
                 3 = Ranking statistic. See \code{RANKSTAT}}
  \item{DECISION.THRES}{Threshold for \code{DECISION}. Ex 0.05 for FDR (\code{DECISION}=1). }
  \item{RANKSTAT}{Statistic used for ranking the couples:

                 1 = P-value

                 2 = Lower bound of the 95\% two sided confidence interval of log(PRR).}
}
\details{
The FDR is estimated with the LBE procedure proposed by Dalmasso et al. (2005). Note that the FDR can only be estimated if the statistic of interest is the P-value.
}
\value{
  \item{ALLSIGNALS}{Data.frame summarizing the results of all couples with at least \code{MIN.n11} notifications ordered by \code{RANKSTAT}. It contains notably the labels, the cell counts, the expected counts (\eqn{n1. \times n.1 / N}{n1. * n.1 / N}, see \code{\link{as.PhViD}}), \code{RANKSTAT}, the observed relative risks (PRR), the marginal counts and the estimations of FDR (when \code{RANKSTAT=1}.)}
  \item{SIGNALS}{Same Data.frame as \code{ALLSIGNALS} but restricted to the list of generated signals.}
  \item{NB.SIGNALS}{Number of generated signals.} 
  \item{INPUT.PARAM}{Parameters entered in the function.}
}
\references{
Ahmed I, Dalmasso C, Haramburu F, Thiessard F, \enc{Broët}{Broet} P, Tubert-Bitter P. False discovery rate estimation for frequentist pharmacovigilance signal detection methods. Biometrics. 2010 Mar;66(1):301-309. 

Dalmasso C, \enc{Broët}{Broet} P, Moreau T (2005), A simple procedure for estimating the false discovery rate, Bioinformatics, Bioinformatics, 21: 660 - 668.

Evans SJ, Waller PC, Davis S, Use of Proportional Reporting Ratios (PRRs) for Signal Generation from Spontaneous Adverse Drug Reaction Reports Pharmacoepidemiology and Drug Safety, 2001, 10, 483-486. 

van Puijenbroek EP, Bate A, Leufkens HGM, Lindquist M, Orre R and Egberts ACG, A comparison of measures of disproportionality for signal detection in spontaneous reporting systems for adverse drug reactions, Pharmacoepidemiology and Drug Safety, 2002, 11, 3-10.



}
\examples{
## start
data(PhViDdata.frame)
PhViDdata <- as.PhViD(PhViDdata.frame)
res <- PRR(PhViDdata)
## end
}
\author{\enc{Ismaïl}{Ismail} Ahmed & Antoine Poncet}
\keyword{ htest }
