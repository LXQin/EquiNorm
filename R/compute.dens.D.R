compute.dens.D <-
function(data.y.simple, data.x, sigma2.hat, tau2.hat, phi2.hat, mu.hat){
   n			<- ncol(data.y.simple)
   n1			<- sum(data.x)

   # gene effect
   mu			<- rep(0, n)
   temp		<- cbind(rep(1, n))
   sigma		<- diag(1,n)*sigma2.hat + temp%*%t(temp)*tau2.hat

   # treatment effect
   mu[data.x==1]	<- mu.hat
   temp		<- cbind(rep(1, n1))
   sigma[data.x==1, data.x==1]	<- sigma[data.x==1, data.x==1] + temp%*%t(temp)*phi2.hat

   dens1		<- apply(data.y.simple, 1, dmvnorm, mean=mu, sigma=sigma, log=TRUE)

   dens1
}

