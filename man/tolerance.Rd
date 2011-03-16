\name{tolerance}
\alias{tolerance}
\docType{data}
\title{Tolerance for EquiNorm convergence.}

\description{(Optional) The function equi.gene.norm() iteratively estimates an array effect (alpha.hat[i]) for each sample.  Algorithm convergence is obtained when the sum of the absolute differences of the array effects from one iteration to the next is less than the specified tolerance value. That is, iterations continue until sum(abs(alpha.hat - alpha.old)) < tolerance (or, alternatively, when the maximum number of iterations is reached see maxIter). The default value is 1E-3.}

\keyword{argument}