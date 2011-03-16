\name{equi.gene.norm}
\alias{equi.gene.norm}
\title{Normalize expression data using equivalently expressed genes}

\description{Normalize oligonucleotide gene expression data using equivalently expressed genes in experiments comparing two groups of samples (for example, tumor samples vs. normal samples), as outlined in LX Qin and JM Satagopan (2009).
}

\usage{
equi.gene.norm( data.y, data.x,
                pi.start = 0.1, lambda.start = 0.9, tolerance = 1E-3, maxIter = 10,
				lemma.outdir = "OUT", lemma.tol = 1E-6, lemma.maxIts = 50000, lemma.plots = FALSE )
}

\arguments{

  \item{data.y}{Array of observed probe-level gene expressions. This is a 3-dimensional array containing probe-level gene expression data.  The array data.y contains values for \emph{S} probesets, \emph{n} samples, and \emph{P} probes}
  
  \item{data.x}{Vector indicating sample group (e.g. disease status with 0 for Control, 1 for Case). The vector data.x is a binary vector of length \emph{n}, where \emph{n} is the number of samples.}

  \item{pi.start}{(Optional) Starting value for the proportion of differentially expressed genes. The default value is 0.1.}
  
  \item{lambda.start}{(Optional) Starting value for the proportion of over-expressed genes among differentially expressed genes. The default value is 0.9.}
  
  \item{tolerance}{(Optional) The function equi.gene.norm() iteratively estimates an array effect (alpha.hat[i]) for each sample.  Algorithm convergence is obtained when the sum of the absolute differences of the array effects from one iteration to the next is less than the specified tolerance value. That is, iterations continue until sum(abs(alpha.hat - alpha.old)) < tolerance (or, alternatively, when the maximum number of iterations is reached see maxIter). The default value is 1E-3.}
  
  \item{maxIter}{(Optional)The maximum number of iterations for equi.gene.norm(). The default value is 10.}
  
  \item{lemma.outdir}{(Optional) The function equi.gene.norm() implements an EM algorithm to estimate array effects and classify genes as non-differentially expressed, under-expressed, or over-expressed.  The M-step of this EM algorithm calls the lemma() function in the lemma library to estimate an auxiliary EM algorithm, which actually does the gene classification.  The following parameters are passed to lemma(). For further information see the documentation for the lemma package. lemma.outdir specifies the output directory in which results from lemma() estimation are stored. The specified directory is a subdirectory of the current working directory. The default value is "OUT". This will create a folder called OUT in the current working directory.}
  
  \item{lemma.tol}{(Optional)This is the tolerance parameter for lemma() and it determines how many iterations of the EM algorithm must be performed.  The default value is 1E-6.}
  
  \item{lemma.maxIts}{(Optional)This is the maximum number of iterations for lemma(). The default value is 50000.}
  
  \item{lemma.plots}{(Optional) This parameter specifies whether the lemma() function should generate diagnostic plots. The default value is FALSE.}  
}

\details{
The function equi.gene.norm() implements a hierarchical mixture model to classify genes as differentially expressed or equivalently expressed and to simultaneously normalize the array data by estimating an array effect for each sample and then subtracting out these array effects. The gene classification step is estimated via the Laplace-approximated expectation-maximization algorithm (LEMMA) for a mixture of Gaussian distributions. This model is implemented in the lemma R library.  Included in the output from the lemma() function is a prediction for each gene indicating whether it is non-differentially expressed, under-expressed, or over-expressed.

Thus, the EM algorithm implemented in equi.gene.norm() consists of iterating between (1) estimating and removing sample-specific array effects, and (2) predicting expression group for each gene via an auxiliary EM algorithm implemented in the lemma package. Once the model converges, we estimate a mixed effects model regressing mean expression level (y) of each probe on binary variables indicating over-expression and under-expression (xo and xu, respectively).

The function equi.gene.norm() returns a list est.hat containing the estimates for the model parameters and predicted expression group for each gene. More details are available in the documentation for est.hat.
}

\note{This algorithm executes relatively quickly because the implementation in the lemma library is very efficient.  However, there may be a limit to the number of samples that can be estimated reliably. Larger datasets may cause the algorithm to fail.  Our simulations suggests the limit for Windows machines is approximately fifty samples, and the limit for Linux machines is approximately 120 samples. Of course this depends on the particular data and the memory/computational power available.  Additionally, several warning messages may be returned when working with larger datasets.  Two such warnings are: "In objective(.par, ...) : value out of range in 'gammafn'" and "In sqrt(M[2, 2]) : NaNs produced."  These are merely warnings (not errors) and do not appear to affect the accuracy of the results.
}

\value{
  \item{est.hat}{List containing the estimates of theta.hat (model parameters) and e.hat (prediction of over-expression or under-expression).}
}

\references{Qin LX and Satagopan J. Normalization method for transcriptional studies of heterogeneous samples - simultaneous array normalization and identification of equivalent expression. Statistical Applications in Genetics and Molecular Biology 2009, 8: Article 10.\cr
\cr
Haim Bar and Elizabeth Schifano. (2010). lemma: Laplace approximated EM Microarray Analysis. R package version 1.3-1.  \url{http://CRAN.R-project.org/package=lemma}\cr
\cr
Douglas Bates <bates@stat.wisc.edu> and Martin Maechler <maechler@R-project.org> (2010). lme4: Linear mixed-effects models using S4 classes. R package version 0.999375-37. \url{http://CRAN.R-project.org/package=lme4}\cr
\cr
Douglas Bates <bates@stat.wisc.edu> and Martin Maechler  <maechler@stat.math.ethz.ch> (2010). Matrix: Sparse and Dense Matrix Classes and Methods. R package version 0.999375-46. \url{http://CRAN.R-project.org/package=Matrix}\cr
\cr
Sarkar, Deepayan (2008) Lattice: Multivariate Data Visualization with R. Springer, New York. ISBN 978-0-387-75968-5\cr
}
			
\author{Qin LX and Satagopan J.}

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

