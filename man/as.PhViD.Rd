\name{as.PhViD}
\alias{as.PhViD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{data.frame to PhViD data}
\description{
  \code{as.PhViD} is a function that converts a data.frame into an object that can be used in the signal detection method functions.
}
\usage{
as.PhViD(DATA.FRAME, MARGIN.THRES = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DATA.FRAME}{The \code{data.frame} has to be structured as follows

			1st column: label of the drugs

			2nd column: label of the adverse events

			3rd column: Number of spontaneous reports of the corresponding couple drug-adverse event.
}
  \item{MARGIN.THRES}{ This option can be used to eliminate the drugs and the adverse events for which the marginal counts are less than \code{MARGIN.THRES}.}
}
\value{
  \item{L}{\code{data.frame} that contains the labels of the drugs and the adverse events.}
  \item{N}{sum of the spontaneous reports counts.}
  \item{data}{\code{data.frame} that contains the number of spontaneous reports (\code{n11}) and the corresponding marginal counts as well (\code{n1.} and \code{n.1}).}
}

\author{Ismail Ahmed & Antoine Poncet}
\keyword{ manip }
