\name{PhViDdata.frame}
\alias{PhViDdata.frame}
\docType{data}
\title{Simulated Pharmacovigilance data}
\description{
This is a simulated data set aiming at mimicking the French database coded in ATC5 for the drugs and HLT for the adverse events. The simulation procedure is described in Ahmed et al.
}
\usage{data(PhViDdata.frame)}
\format{
  A data frame with 102483 observations on the following 3 variables.
  \describe{
    \item{\code{Drug lab}}{a factor indicating the label of the 634 drugs.}
    \item{\code{AE lab}}{a factor indicating the label of the 756 adverse events.}
    \item{\code{n11}}{a numeric vector indicating the number of spontaneous reports of the corresponding couple.}
  }
}
\references{
Ahmed I, Haramburu F, Fourrier-Reglat A, Thiessard F,  Kreft-Jais C, Miremont-Salame G, Begaud B, Tubert-Bitter P, Bayesian Pharmacovigilance Signal Detection Methods Revisited in a Multiple Comparison Setting, Statistics in Medicine, accepted.
}
\keyword{datasets}
