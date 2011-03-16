computeAuxillaryParameters <-
function (data.y, data.x, alpha.hat, e.hat) 
{
    G <- dim(data.y)[1]
    n <- length(data.x)
    y <- data.y
    group <- t(apply(rbind(1:G), 2, rep, each = n))
    xo <- matrix(0, nrow = G, ncol = n)
    xo[e.hat$o.hat == 1, data.x == 1] <- 1
    xu <- matrix(0, nrow = G, ncol = n)
    xu[e.hat$u.hat == 1, data.x == 1] <- 1
    data.lme <- data.frame(y = as.vector(y), xo = as.vector(xo), 
        xu = as.vector(xu), group = as.vector(group))
    temp.lm <- lm(y ~ -1 + xo + xu, data = data.lme)
    temp <- lmer(y ~ -1 + xo + xu + (1 + xo + xu | group), data = data.lme, 
        REML = TRUE, start = c(xo = temp.lm$coefficients[[1]], 
            xu = temp.lm$coefficients[[2]]), control = list(maxIter = 3000))
    muO.hat <- fixef(temp)[1]
    muU.hat <- fixef(temp)[2]
    var.hat <- as.double(rbind(diag(VarCorr(temp)$group)[[1]], 
        diag(VarCorr(temp)$group)[[2]], diag(VarCorr(temp)$group)[[3]], 
        attr(VarCorr(temp), "sc")[[1]]^2))
    return(list(muU.hat = muU.hat, muO.hat = muO.hat, tau2.hat = var.hat[1], 
        psi2.hat = var.hat[2], xi2.hat = var.hat[3], sigma2.hat = P * 
            var.hat[4]))
}
