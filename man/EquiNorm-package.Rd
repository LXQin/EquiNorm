\name{EquiNorm-package}
\alias{EquiNorm-package}
\alias{EquiNorm}
\docType{package}
\title{Normalize expression data using equivalently expressed genes}

\description{Normalize oligonucleotide gene expression data using equivalently expressed genes in experiments comparing two groups of samples (for example, tumor samples vs. normal samples), as outlined in LX Qin and JM Satagopan (2009).
}

\details{
\tabular{ll}{
Package: \tab EquiNorm\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2011-02-08\cr
License: \tab LGPL >= 2.0\cr
LazyLoad: \tab yes\cr
}

Existing methods for data normalization often assume that there are few or symmetric differential expression, but this assumption does not always hold. Alternatively, non-differentially expressed genes may be used for array  normalization. However, it is unknown a priori which genes are non-differentially expressed. This procedure implements a hierarchical mixture model framework to simultaneously identify non-differentially expressed genes and normalize arrays using these genes.

The required inputs are an array of gene expression data (data.y) and a binary sequence indicating the disease status of each subject (data.x). Additional parameters are described in the documentation for the equi.gene.norm() function.
}

\author{ Li-Xuan Qin \email{qinl@mskcc.org}, Jaya M. Satagopan \email{satagopj@mskcc.org}\cr
Maintainer: Brian Denton \email{dentonb@mskcc.org}
}

\references{Qin LX and Satagopan J. Normalization method for transcriptional studies of heterogeneous samples - simultaneous array normalization and identification of equivalent expression. Statistical Applications in Genetics and Molecular Biology 2009, 8: Article 10.\cr
\cr
Haim Bar and Elizabeth Schifano. (2010). lemma: Laplace approximated EM Microarray Analysis. R package version 1.3-1.  \url{http://CRAN.R-project.org/package=lemma}\cr
\cr
Douglas Bates <bates@stat.wisc.edu> and Martin Maechler <maechler@R-project.org> (2010). lme4: Linear mixed-effects models  using S4 classes. R package version 0.999375-37. \url{http://CRAN.R-project.org/package=lme4}\cr
\cr
Douglas Bates <bates@stat.wisc.edu> and Martin Maechler  <maechler@stat.math.ethz.ch> (2010). Matrix: Sparse and Dense Matrix Classes and Methods. R package version 0.999375-46. \url{http://CRAN.R-project.org/package=Matrix}\cr
\cr
Sarkar, Deepayan (2008) Lattice: Multivariate Data Visualization with R. Springer, New York. ISBN 978-0-387-75968-5\cr
}
			
\examples{
################################
# Load data.simulated data     #
################################

data(data.simulated)

########################################################
# Run equi.gene.norm function.                         #
# data.x and data.y are the only required parameters;  #
# all others are optional.                             #
########################################################

est.hat.lemma <- equi.gene.norm( data.simulated$data.y, data.simulated$data.x,
				 pi.start = 0.1, lambda.start = 0.9, tolerance = 1E-3, maxIter = 10,
				 lemma.outdir = "OUT", lemma.tol = 1E-6, lemma.maxIts = 50000, lemma.plots = FALSE )

###########################################################################
# Print results from estimation and compare with true values.             #
# The true values are known because the example data are data.simulated.  #
###########################################################################

# alpha.hat is an n-element list containing the estimated array effect for each of the arrays.
# pi.hat is the estimated proportion of genes that are differentially expressed.
# lambda.hat is the estimated proportion of differentially expressed genes that are over-expressed.
# muO.hat is the treatment effect of gene expression among over-expressed genes.
# muU.hat is the treatment effect of gene expression among under-expressed genes.
# tau2.hat is the variance of gene effects.
# psi2.hat is the variance of treatment effect for over-expressed genes.
# xi2.hat is the variance of the treatment effect for under-expressed genes.
# sigma2.hat is the variance of measurement error.

est.hat.lemma$theta.hat

# Percent error for parameter estimates
100*(est.hat.lemma$theta.hat$pi.hat - data.simulated$true.values$pi)/data.simulated$true.values$pi
100*(est.hat.lemma$theta.hat$lambda.hat - data.simulated$true.values$lambda)/data.simulated$true.values$lambda
100*(est.hat.lemma$theta.hat$muU.hat - data.simulated$true.values$muU)/data.simulated$true.values$muU
100*(est.hat.lemma$theta.hat$muO.hat - data.simulated$true.values$muO)/data.simulated$true.values$muO
100*(est.hat.lemma$theta.hat$tau2.hat - data.simulated$true.values$tau2)/data.simulated$true.values$tau2
100*(est.hat.lemma$theta.hat$psi2.hat - data.simulated$true.values$psi2)/data.simulated$true.values$psi2
100*(est.hat.lemma$theta.hat$xi2.hat - data.simulated$true.values$xi2)/data.simulated$true.values$xi2
100*(est.hat.lemma$theta.hat$sigma2.hat - data.simulated$true.values$sigma2)/data.simulated$true.values$sigma2

# Percent error for estimated array effects
alpha.compare <- cbind(data.simulated$data.x, data.simulated$true.values$alpha, est.hat.lemma$theta.hat$alpha.hat,
                       100*(est.hat.lemma$theta.hat$alpha.hat-data.simulated$true.values$alpha)/data.simulated$true.values$alpha )
colnames(alpha.compare) <- c("data.simulated$data.x", "True alpha","alpha.hat","Percent Error" )
alpha.compare

# Plot of true array effects versus estimated array effects

Control.count <- length( which( data.simulated$data.x == 0 ) )
Case.count <- length( which( data.simulated$data.x == 1 ) )

plot( x = data.simulated$true.values$alpha, y = est.hat.lemma$theta.hat$alpha.hat,
      xlab = "True alpha", ylab = "Estimated alpha",
      main = "Plot of True alpha vs. Estimated alpha",
	  col = c(rep("black",Control.count), rep("blue",Case.count) ) )
abline( a = 0, b = 1, col = "red" )
legend( "bottomright", pch = c(1,1), col = c("black", "blue" ),
         legend = c("Control","Case") )

# Cross-tabulations for predictions of under-expression and over-expression

table( data.simulated$true.values$data.u, est.hat.lemma$e.hat$u.hat )
prop.table(table( data.simulated$true.values$data.u, est.hat.lemma$e.hat$u.hat ))

table( data.simulated$true.values$data.o, est.hat.lemma$e.hat$o.hat )
prop.table(table( data.simulated$true.values$data.o, est.hat.lemma$e.hat$o.hat ))
}
