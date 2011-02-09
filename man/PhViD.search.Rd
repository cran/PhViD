\encoding{UTF-8}
\name{PhViD.search}
\alias{PhViD.search}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PhViD.search}
\description{
This function makes possible to extract some information from the output of the PhViD functions for a given couple adverse event-drug, for a drug or for an adverse event.
}
\usage{
PhViD.search(RESULT, DRUG = NULL, EVENT = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{RESULT}{\code{RESULT} must be the output of one the signal detection method functions (\code{ROR, PRR, RFET, GPS} or \code{BCPNN}}
  \item{DRUG}{The label of the drug. By default, \code{DRUG=FALSE}.}
  \item{EVENT}{The label of the adverse event. By default, \code{EVENT=FALSE}.}
}

\value{
  \item{DRUG }{Recalls the label of the drug.}
  \item{EVENT}{Recalls the label of the event.}
  \item{EXIST_DRUG }{Indicates if the label of the drug exists in the database.}
  \item{EVENT}{Indicates if the label of the adverse event exists in the database.}
  \item{EXIST_COUPLE}{Indicates if the couple is present in the database.}
  \item{LIST}{It is a \code{dataframe} that contains the labels, the counts, the expected counts, the value of the statistic of interest, the rank and the estimated FDR for each couple.}
}

\author{Antoine Poncet & \enc{Ismaïl}{Ismail} Ahmed}
\keyword{ manip }
