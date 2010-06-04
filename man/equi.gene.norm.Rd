\name{equi.gene.norm}
\alias{equi.gene.norm}
\title{Normalize expression data using equivalently expressed genes}

\description{Normalize oligonucleotide gene expression data using equivalently expressed genes in experiments comparing two groups of samples (for example, tumor samples vs. normal samlpes), as outlined in LX Qin and JM Satagopan (2009).
}

\usage{equi.gene.norm( data.y, data.x, pi.start, lambda.start)}

\arguments{
  \item{data.y}{Array of observed probe-level gene expressions.}
  \item{data.x}{Vector indicating disease sample group (0 for group 1, 1 for group 2).}
  \item{pi.start}{Starting value for the proportion of differentially expressed genes.}
  \item{lambda.start}{Starting value for proportion of over-expressed genes among differentially expressed genes.}
}

\details{
The function equi.gene.norm() implements a hierarchical mixture model to 
classify genes as differentially expressed or equivalently expressed 
and to simultaneously normalize the array data. The model is estimated via the
expectation-maximation (EM) algorithm for a mixture of Gaussian distributions.

The central model estimated in the M-step of the EM algorithm is a mixed effects model that 
regresses expression level (y) on binary variables indicating over-expression 
and under-expression (xo and xu, respectively). The intercept term is supressed 
in the fixed effects portion of the model. The random effects are similarly defined, 
with the exception that an intercept is included and the random effects are nested.  For 
further details refer to Qin LX and Satagopan J. 2009. In particular, see Section 7.1.

lmer( y ~ -1 + xo + xu + (1+xo+xu|geneGroup) ),   where geneGroup = g*( 1 - 2*(xo+xu) )

For each iteration of the EM algorithm, the estimated predictions for the number
of over-expressed and under-expressed genes are printed in a 2x2 table (i.e. a
crosstabulation of o.hat vs. u.hat). Also printed is the log-likelihood, and the 
estimates for the standard errors of the random intercept term, and random effects 
for over-expressed and under-expressed genes in the mixed effects model, as well as 
the standard error of the model residuals.  These statistics are provided as a means 
to moniter the algorithm's progression to convergence.

Please note that this algorithm is very computationally demanding and may take several 
hours to complete.  As an example, we executed the algorithm on an ovarian cancer 
dataset containing 22,277 probesets, 322 samples, and 11 probes per probeset.  The algorithm took 
approximately 33 hours to complete on a GNU/Linux machine running Ubuntu 9.10 with 
a 2.33 GHz Intel Xeon E5345 processor and 32 GB of RAM.
}

\value{
  \item{est.hat}{List containing the estimates of theta.hat (model parameters) and e.hat (prediction of over-expression or under-expression).}
}

\references{Normalization method for transcriptional studies of heterogeneous samples - simultaneous array normalization and identification of equivalent expression. Statistical Applications in Genetics and Molecular Biology 2009, 8: Article 10.
}
			
\author{Qin LX and Satagopan J.}

\examples{equi.gene.norm( data.y, data.x, pi.start = 0.1, lambda.start = 0.9 )}

