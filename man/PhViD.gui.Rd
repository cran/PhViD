\name{PhViD.gui}
\alias{PhViD.gui}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphical User Interface for the PhViD package}
\description{
This function makes possible the use of all the signal detection methods through a friendly user interface. The data have to be in a .csv file, ie with ";" as column separator (see \code{\link{read.table}}) and must contain the label of the column. The first two columns must be respectively the labels of the drugs and the adverse events. The third column is the number of spontaneous reports corresponding to the couple involved.
}
\usage{
PhViD.gui(dummy = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dummy}{No argument is required.}
}
\details{
The GUI is simply called by \code{PhViD.gui().}
}
\references{Ahmed I, Dalmasso C, Haramburu F, Thiessard F, Broet P, Tubert-Bitter P, False Discovery Rate Estimation for Frequentist Pharmacovigilance Signal Detection Methods, Biometrics, accepted.

Ahmed I, Haramburu F, Fourrier-Reglat A, Thiessard F,  Kreft-Jais C, Miremont-Salame G, Begaud B, Tubert-Bitter P, Bayesian Pharmacovigilance Signal Detection Methods Revisited in a Multiple Comparison Setting, Statistics in Medicine, accepted.

Bate A, Lindquist M, Edwards IR, Olsson S, Orre R, Lansner A, De Freitas RM. A Bayesian Neural Network Method for Adverse Drug Reaction Signal Generation European Journal of Clinical Pharmacology, 1998, 54, 315-321.

Dalmasso C, Broet P, Moreau T (2005), A simple procedure for estimating the false discovery rate, Bioinformatics, Bioinformatics, 21: 660 - 668.

DuMouchel W. Bayesian Data Mining in Large Frequency Tables, with an Application to the FDA Spontaneous Reporting System. The American Statistician. 1999, 53. 177-190.

Evans SJ, Waller PC, Davis S, Use of Proportional Reporting Ratios (PRRs) for Signal Generation from Spontaneous Adverse Drug Reaction Reports Pharmacoepidemiology and Drug Safety, 2001, 10, 483-486.

Noren, GN, Bate A, Orre R, Edwards IR, Extending the methods used to screen the WHO drug safety database towards analysis of complex associations and improved accuracy for rare events Statistics in Medicine, 2006, 25, 3740-3757.

van Puijenbroek EP, Bate A, Leufkens HG, Lindquist M, Orre R, Egberts AC, A Comparison of Measures of Disproportionality for Signal Detection in Spontaneous Reporting Systems for Adverse Drug Reactions Pharmacoepidemiology and Drug Safety, 2002, 11, 3-1.
 }

\author{Antoine Poncet & Ismail Ahmed}
\keyword{ htest }
