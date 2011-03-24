\name{est.hat}
\alias{est.hat}
\docType{data}
\title{List containing the estimates from the hierarchical mixture model.}

\description{List containing the estimates from the hierarchical mixture model.}

\format{
The format is:\cr
 
List of 2\cr
 
\$ \bold{theta.hat}   : List of 10\cr
\$ alpha.hat : num [1:\emph{n}] 8.14 8.04 7.85 7.19 8.21 ...\cr
\$ pi.hat    : num 0.32\cr
\$ lambda.hat: num 0.37\cr
\$ muO.hat   : num 0.355\cr
\$ muU.hat   : num -0.0523\cr
\$ tau2.hat  : num 1.25\cr
\$ psi2.hat  : num 4.41e-06\cr
\$ xi2.hat   : num 0.767\cr
\$ sigma2.hat: num 0.0516\cr
\$ llh       : num -50373\cr
	
\$ \bold{e.hat}       : List of 2\cr
\$ o.hat: int [1:\emph{S}] 0 0 0 0 0 0 0 0 0 1 ...\cr
\$ u.hat: int [1:\emph{S}] 1 0 1 0 0 0 0 0 1 0 ...
}

\details{
\bold{theta.hat} is a 10-element list containing the following: \cr
  alpha.hat is an \emph{n}-element list containing the estimated array effect for each of the arrays.\cr
  pi.hat is the estimated proportion of genes that are differentialy expressed.\cr
  lambda.hat is the estimated proportion of differentially expressed genes that are over-expressed.\cr
  muO.hat is the treatment effect of gene expression among over-expressed genes.\cr
  muU.hat is the treatmemt effect of gene expression among under-expressed genes.\cr
  tau2.hat is the variance of gene effects.\cr
  psi2.hat is the variance of treatment effect for over-expressed genes.\cr
  xi2.hat is the variance of the treatment effect for under-expressed genes.\cr
  sigma2.hat is the variance of measurement error.\cr
  llh is the log-likelihood.\cr

\bold{e.hat} is a 2-element list containing the following:\cr
  o.hat is an \emph{S}-element list with value equal 1 if that probeset is predicted to be \emph{over}-expressed and 0 otherwise.\cr
  u.hat is an \emph{S}-element list with value equal 1 if that probeset is predicted to be \emph{under}-expressed and 0 otherwise.\cr

Where \emph{S} is the number of probesets and \emph{n} is the number of samples in the dataset.
}
\keyword{ datasets }
