initialize.e <-
function (data.y, data.x, pi.start, lambda.start) 
{
    pi.hat <- pi.start
    lambda.hat <- lambda.start
    u.hat <- NULL
    o.hat <- NULL
    for (i in 1:dim(data.y)[[1]]) {
        prob <- runif(1, min = 0, max = 1)
        if (prob < pi.hat) {
            u.hat[i] <- 0
            o.hat[i] <- 0
        }
        else if (prob <- pi.hat + pi.start * lambda.hat) {
            u.hat[i] <- 0
            o.hat[i] <- 1
        }
        else {
            u.hat[i] <- 1
            o.hat[i] <- 0
        }
    }
    z.hat <- u.hat + o.hat
    return(list(u.hat = u.hat, o.hat = o.hat, z.hat = z.hat))
}
