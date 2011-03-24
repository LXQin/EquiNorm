initialize.theta <-
function (data.y, data.x, pi.start, lambda.start) 
{
    pi.hat <- pi.start
    lambda.hat <- lambda.start
    alpha.hat <- apply(data.y, 2, mean)
    theta.hat <- list(alpha.hat = alpha.hat, pi.hat = pi.hat, 
        lambda.hat = lambda.hat)
    return(theta.hat)
}
