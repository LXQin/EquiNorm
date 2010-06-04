compute.dens.ND <-
function(data.y.simple, sigma2.hat, tau2.hat){
   n			<- ncol(data.y.simple)
   temp		<- cbind(rep(1, n))
   mu			<- rep(0, n)
   sigma		<- diag(1,n)*sigma2.hat + temp%*%t(temp)*tau2.hat
   dens0		<- apply(data.y.simple, 1, dmvnorm, mean=mu, sigma=sigma, log=TRUE)

   dens0
}

