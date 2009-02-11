\name{RFET}
\alias{RFET}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reporting Fisher's Exact Test}
\description{
This function proposes the Fisher's Exact Test as an alternative to the PRR and ROR methods. The statistic of interest is the P-value or the mid-P-value resulting from the test (Ahmed et al., Biometrics).
}
\usage{
RFET(DATABASE, OR0 = 1, MIN.n11 = 1, DECISION = 1, 
DECISION.THRES = 0.05, MID.PVAL = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DATABASE}{Object returned by the function \code{as.PhViD}.}
  \item{OR0}{Value of the tested odds ratio. By default, \code{OR0=1}.}
  \item{MIN.n11}{Minimum number of notifications for a couple to be potentially considered as a signal. By default, \code{MIN.n11 = 1}.}
  \item{DECISION}{Decision rule for the signal generation based on

                 1 = FDR (Default value)

                 2 = Number of signals

                 3 = P-values or mid-P-values. See \code{MID.PVAL}}
  \item{DECISION.THRES}{Threshold for \code{DECISION}. Ex 0.05 for FDR (\code{DECISION}=1). }
  \item{MID.PVAL}{if \code{MID.PVAL=TRUE}, the statistic of interest becomes the mid-P-values instead of the P-values resulting from the Fisher's exact test. By default \code{MID.PVAL=FALSE}.}
}
\details{
The FDR is estimated with the LBE procedure proposed by Dalmasso et al. (2005).
}
\value{
  \item{ALLSIGNALS}{Data.frame summarizing the results of all couples with at least \code{MIN.n11} notifications ordered by \code{RANKSTAT}. It contains notably the labels, the cell counts, the expected count (\eqn{n1. \times n.1 / N}{n1. * n.1 / N}, see \code{\link{as.PhViD}}), \code{RANKSTAT}, the observed odds ratio (ROR), the marginal counts and the estimation of FDR.}
  \item{SIGNALS}{Same Data.frame as \code{ALLSIGNALS} but restricted to the list of generated signals.}
  \item{NB.SIGNALS}{Number of generated signals.} 
  \item{INPUT.PARAM}{Parameters entered in the function.}
}
\references{
Ahmed I, Dalmasso C, Haramburu F, Thiessard F, Broet P, Tubert-Bitter P, False Discovery Rate Estimation for Frequentist Pharmacovigilance Signal Detection Methods, Biometrics, accepted.

Dalmasso C, Broet P, Moreau T (2005), A simple procedure for estimating the false discovery rate, Bioinformatics, Bioinformatics, 21: 660 - 668. 

}
\examples{
## start
#data(PhViDdata.frame)
#PhViDdata <- as.PhViD(PhViDdata.frame)
#res <- RFET(PhViDdata)
## end
}

\author{Ismail Ahmed & Antoine Poncet}
\keyword{ htest }
