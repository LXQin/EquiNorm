\name{data.simulated}
\alias{data.simulated}
\docType{data}
\title{A list containing simulated datasets.}

\description{A list containing simulated datasets - i.e. an example data.x and data.y.}

\format{
The format is:\cr
List of 3\cr
\$ data.y     : num [1:\emph{S}, 1:\emph{n}, 1:\emph{P}] 25.2 20.5 27.2 25.1 22.7 ...\cr
\cr
\$ data.x     : int [1:\emph{n}] 0 0 0 0 0 0 0 0 0 0 ...\cr
\cr
\$ \bold{true.values} : List of 11\cr
\$ lambda: num 0.2\cr
\$ pi    : num 0.1\cr
\$ muU   : num -2\cr
\$ muO   : num 2\cr
\$ tau2  : num 6.25\cr
\$ psi2  : num 1\cr
\$ xi2   : num 0.562\cr
\$ sigma2: num 0.09\cr
\$ alpha : num [1:\emph{n}] 24.6 22.1 22 21.9 21.6 ...\cr
\$ data.o: num [1:\emph{S}] 1 1 1 1 1 1 1 1 1 1 ...\cr
\$ data.u: num [1:\emph{S}] 0 0 0 0 0 0 0 0 0 0 ...\cr
}

\details{
data.y is a three-dimensional array of simulated expression data from 20 samples, 500 probesets, and 11 probes per probeset.\cr
\cr
data.x is a binary vector indicating disease status for each of 20 simulated samples.\cr
\cr
\bold{true.values} is an 11-element list containing the following paramters used to simulate data.y and data.x: \cr
  alpha is a 20-element list containing the estimated array effect for each of the arrays.\cr
  pi is the estimated proportion of genes  are differentialy expressed.\cr
  lambda is the estimated proportion of differentially expressed genes  are over-expressed.\cr
  muO is the treatment effect of gene expression among over-expressed genes.\cr
  muU is the treatmemt effect of gene expression among under-expressed genes.\cr
  tau2 is the variance of gene effects.\cr
  psi2 is the variance of treatment effect for over-expressed genes.\cr
  xi2 is the variance of the treatment effect for under-expressed genes.\cr
  sigma2 is the variance of measurement error.\cr
}

\keyword{ datasets }