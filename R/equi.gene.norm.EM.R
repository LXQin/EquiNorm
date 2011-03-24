equi.gene.norm.EM <-
function (data.y, data.x, theta.hat, e.hat, tolerance, maxIter, 
    lemma.outdir, lemma.tol, lemma.maxIts, lemma.plots) 
{
    data.y.start <- data.y
    alpha.hat <- rep(Inf, n)
    alpha.old <- rep(-Inf, n)
    iter <- 0
    while (sum(abs(alpha.hat - alpha.old)) > tolerance & iter < 
        maxIter) {
        alpha.old <- alpha.hat
        iter <- iter + 1
        Estep.results <- equi.gene.norm.Estep(data.y, data.x, 
            theta.hat, e.hat)
        data.y <- Estep.results$data.y
        alpha.hat <- Estep.results$alpha.hat
        e.hat <- equi.gene.norm.Mstep(data.y, data.x, theta.hat, 
            e.hat, lemma.outdir, lemma.tol, lemma.maxIts, lemma.plots)
        cat(paste("Iteration:", iter, "\tsum(abs(alpha.hat - alpha.old)):", 
            sum(abs(alpha.hat - alpha.old)), "\n\n\n"))
        if (iter == maxIter) {
            print("Maximim number of iterations reached")
        }
        else if (sum(abs(alpha.hat - alpha.old)) <= tolerance) {
            print("alpha.hat convergence criterion met")
        }
        flush.console()
    }
    theta.hat$pi.hat <- mean(e.hat$z.hat)
    theta.hat$lambda.hat <- mean(e.hat$o.hat)/theta.hat$pi.hat
    parms.hat <- computeAuxillaryParameters(data.y, data.x, alpha.hat, 
        e.hat)
    theta.hat$muU.hat <- parms.hat$muU.hat
    theta.hat$muO.hat <- parms.hat$muO.hat
    theta.hat$xi2.hat <- parms.hat$xi2.hat
    theta.hat$psi2.hat <- parms.hat$psi2.hat
    theta.hat$tau2.hat <- parms.hat$tau2.hat
    theta.hat$sigma2.hat <- parms.hat$sigma2.hat
    return(list(theta.hat = theta.hat, e.hat = e.hat))
}
