\name{ROR}
\alias{ROR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reporting Odds Ratio}
\description{
Reporting Odds Ratio proposed by van Puijenbroak et al. (2002) extended to the multiple comparison framework.
}
\usage{
ROR(DATABASE, OR0 = 1, MIN.n11 = 1, DECISION = 1, 
DECISION.THRES = 0.05, RANKSTAT = 1)
}
%- maybe also 'usage' for other objects documented here.
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DATABASE}{Object returned by the function \code{as.PhViD}.}
  \item{OR0}{Value of the tested odds ratio. By default, \code{OR0=1}.}
  \item{MIN.n11}{Minimum number of notifications for a couple to be potentially considered as a signal. By default, \code{MIN.n11 = 1}.}
  \item{DECISION}{Decision rule for the signal generation based on
  
                 1 = FDR (Default value) 

                 2 = Number of signals 

                 3 = Ranking statistic. See \code{RANKSTAT}}
  \item{DECISION.THRES}{Threshold for \code{DECISION}. Ex 0.05 for FDR (\code{DECISION}=1). }
  \item{RANKSTAT}{Statistic used for ranking the couples: 

                 1 = P-value 

                 2 = Lower bound of the 95\% two sided confidence interval of log(ROR).}
}
\details{
The FDR is estimated with the LBE procedure proposed by Dalmasso et al. (2005). Note that the FDR can only be estimated if the statistic of interest is the P-value.
}
\value{
  \item{ALLSIGNALS}{Data.frame summarizing the results of all couples with at least \code{MIN.n11} notifications ordered by \code{RANKSTAT}. It contains notably the labels, the cell counts, the expected counts (\eqn{n1. \times n.1 / N}{n1. * n.1 / N}, see \code{\link{as.PhViD}}), \code{RANKSTAT}, the observed odds ratios (ROR), the marginal counts and the estimations of FDR (when 
\code{RANKSTAT=1}.)}
  \item{SIGNALS}{Same Data.frame as \code{ALLSIGNALS} but restricted to the list of generated signals.}
  \item{NB.SIGNALS}{Number of generated signals.} 
  \item{INPUT.PARAM}{Parameters entered in the function.}
}
\references{
Ahmed I, Dalmasso C, Haramburu F, Thiessard F, Broet P, Tubert-Bitter P, False Discovery Rate Estimation for Frequentist Pharmacovigilance Signal Detection Methods, Biometrics, accepted.

Dalmasso C, Broet P, Moreau T (2005), A simple procedure for estimating the false discovery rate, Bioinformatics, Bioinformatics, 21: 660 - 668.

van Puijenbroek EP, Bate A, Leufkens HG, Lindquist M, Orre R, Egberts AC, A Comparison of Measures of Disproportionality for Signal Detection in Spontaneous Reporting Systems for Adverse Drug Reactions Pharmacoepidemiology and Drug Safety, 2002, 11, 3-1.
}
\examples{
## start
data(PhViDdata.frame)
PhViDdata <- as.PhViD(PhViDdata.frame)
res <- ROR(PhViDdata, MIN.n11 = 3)

# Decision rule proposed by van Puijenbroek et al. (2002)
res2 <- ROR(PhViDdata, MIN.n11 = 1, DECISION=3, DECISION.THRES=0, RANKSTAT=2)
## end
}

\author{Ismail Ahmed & Antoine Poncet}
\keyword{ htest }
