equi.gene.norm.Estep <-
function (data.y, data.x, theta.hat, e.hat) 
{
    alpha.hat <- theta.hat$alpha.hat
    u.hat <- e.hat$u.hat
    o.hat <- e.hat$o.hat
    z.hat <- e.hat$z.hat
    mean.expression.nondiff <- apply(data.y[z.hat == 0, ], 2, 
        mean)
    data.y <- compute.resid.ctd(data.y, mean.expression.nondiff)
    mean.expression <- mean(data.y)
    mean.expression.controls <- mean(data.y[, data.x == 0])
    mean.expression.cases <- mean(data.y[, data.x == 1])
    delta.alpha <- ifelse(data.x == 0, mean.expression - mean.expression.controls, 
        mean.expression - mean.expression.cases)
    alpha.hat <- alpha.hat + delta.alpha
    return(list(data.y = data.y, alpha.hat = alpha.hat))
}
