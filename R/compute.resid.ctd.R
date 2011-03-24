compute.resid.ctd <-
function (data.y, alpha.hat) 
{
    resid <- t(t(data.y) - alpha.hat)
    return(resid)
}
