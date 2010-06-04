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
Version: \tab 1.0\cr
Date: \tab 2010-06-04\cr
License: \tab LGPL >= 2.0\cr
LazyLoad: \tab yes\cr
}

Existing methods for data normalization often assume that there are few or symmetric differential expression, but this assumption does not always hold.  Alternatively, non-differentially expressed genes may be used for array  normalization. However, it is unknown at the outset which genes are non-differentially expressed.  This procedure implements a hierarchical mixture model framework to simultaneously identify non-differentially expressed genes and normalize arrays using these genes.

The required inputs are an array of gene expression data (data.y), a binary sequence indicating the disease status of each subject (data.x), and starting values for the proportion of differentially expresed genes and the proportion of differentially expressed genes that are over-expressed (pi.start and lambda.start, respectively).
}

\author{ Li-Xuan Qin \email{qinl@mskcc.org}, Jaya M. Satagopan \email{satagopj@mskcc.org}\cr
Maintainer: Brian Denton \email{dentonb@mskcc.org}
}

\references{Qin LX and Satagopan J. Normalization method for transcriptional studies of heterogeneous samples - simultaneous array normalization and identification of equivalent expression. Statistical Applications in Genetics and Molecular Biology 2009, 8: Article 10.
}
			
\examples{
library(mvtnorm)
library(lme4)
equi.gene.norm( data.y, data.x, pi.start = 0.1, lambda.start = 0.9 )
}
