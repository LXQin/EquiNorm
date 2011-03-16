equi.gene.norm <-
function (data.y, data.x, pi.start = 0.1, lambda.start = 0.9, 
    tolerance = 0.001, maxIter = 10, lemma.outdir = "OUT", lemma.tol = 1e-06, 
    lemma.maxIts = 50000, lemma.plots = FALSE) 
{
    S <<- dim(data.y)[[1]]
    n <<- dim(data.y)[[2]]
    P <<- dim(data.y)[[3]]
    data.y <- apply(data.y, 1:2, mean)
    theta.hat <- initialize.theta(data.y, data.x, pi.start, lambda.start)
    e.hat <- initialize.e(data.y, data.x, pi.start, lambda.start)
    est.hat <- equi.gene.norm.EM(data.y, data.x, theta.hat, e.hat, 
        tolerance, maxIter, lemma.outdir, lemma.tol, lemma.maxIts, 
        lemma.plots)
    S <- NULL
    n <- NULL
    P <- NULL
    rm(S, n, P)
    return(est.hat)
}
