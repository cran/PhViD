\name{PhViD-package}
\encoding{UTF-8}
\alias{PhViD-package}
\alias{PhViD}
\docType{package}
\title{PhViD: a R package for PharmacoVigilance signal Detection}
\description{
The \code{PhViD-package} proposes the main pharmacovigilance signal detection methods extended to the multiple comparison setting. These functions can be used as standard R functions or through an user friendly interface (\code{PhViD.gui}). For the frequentist methods, the package requires the LBE procedure that is stored in the Bioconductor website \url{http://bioconductor.org/}. LBE can be installed by entering \cr
\code{source("http://bioconductor.org/biocLite.R")} \cr
\code{biocLite("LBE")} \cr
in the R console.
}

\author{
\enc{Ismaïl}{Ismail} Ahmed & Antoine Poncet

Maintainer: \enc{Ismaïl}{Ismail} Ahmed <ismail.ahmed@inserm.fr>
}
\references{
Ahmed I, Thiessard F, Miremont-Salamé G, Bégaud B, Tubert-Bitter P. Pharmacovigilance data mining with methods based on false discovery rates: a comparative simulation study. Clin. Pharmacol. Ther. 2010 Oct;88(4):492-498. 

Ahmed I, Dalmasso C, Haramburu F, Thiessard F, \enc{Broët}{Broet} P, Tubert-Bitter P. False discovery rate estimation for frequentist pharmacovigilance signal detection methods. Biometrics. 2010 Mar;66(1):301-309. 

Ahmed I, Haramburu F, Fourrier-Réglat A, Thiessard F, Kreft-Jais C, Miremont-Salamé G, Bégaud B, Tubert-Bitter P. Bayesian pharmacovigilance signal detection methods revisited in a multiple comparison setting. Stat Med. 2009 Jun 15;28(13):1774-1792. 

Bate A, Lindquist M, Edwards IR, Olsson S, Orre R, Lansner A, De Freitas RM. A Bayesian Neural Network Method for Adverse Drug Reaction Signal Generation European Journal of Clinical Pharmacology, 1998, 54, 315-321.  

Dalmasso C, Broet P, Moreau T (2005), A simple procedure for estimating the false discovery rate, Bioinformatics, Bioinformatics, 21: 660 - 668. 

DuMouchel W. Bayesian Data Mining in Large Frequency Tables, with an Application to the FDA Spontaneous Reporting System. The American Statistician. 1999, 53. 177-190. 

Evans SJ, Waller PC, Davis S, Use of Proportional Reporting Ratios (PRRs) for Signal Generation from Spontaneous Adverse Drug Reaction Reports Pharmacoepidemiology and Drug Safety, 2001, 10, 483-486. 

Noren, GN, Bate A, Orre R, Edwards IR, Extending the methods used to screen the WHO drug safety database towards analysis of complex associations and improved accuracy for rare events Statistics in Medicine, 2006, 25, 3740-3757. 

van Puijenbroek EP, Bate A, Leufkens HG, Lindquist M, Orre R, Egberts AC, A Comparison of Measures of Disproportionality for Signal Detection in Spontaneous Reporting Systems for Adverse Drug Reactions Pharmacoepidemiology and Drug Safety, 2002, 11, 3-1. 

}
\keyword{ htest }

