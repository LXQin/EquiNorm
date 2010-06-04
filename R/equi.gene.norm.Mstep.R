equi.gene.norm.Mstep <-
function(data.y.simple, data.x, e.hat, theta.hat){

   # delist theta0.hat
   o.hat		<- e.hat$o.hat
   u.hat		<- e.hat$u.hat
   z.hat		<- o.hat + u.hat
   G			<- dim(data.y.simple)[1]
   n			<- dim(data.y.simple)[2]
   
   # frequency of each cluster
   pi.hat 		<- sum(z.hat)/G
   lambda.hat	<- sum(o.hat)/sum(z.hat)

   # estimate alpha.hat (array effect)
   alpha.hat	<- apply(data.y.simple[z.hat==0,], 2, mean)
   
   data.y.simple <- data.y.simple
   
   # xn=non-differential-expression, xo=over-expression, xu=under-expression
   y					<- compute.resid.ctd(data.y.simple, alpha.hat)
   
   group				<- t(apply(rbind(1:G), 2, rep, each=n))
   xo					<- matrix(0, nrow=G, ncol=n)
   xo[o.hat==1, data.x==1]	<- 1
   xu					<- matrix(0, nrow=G, ncol=n)
   xu[u.hat==1, data.x==1]	<- 1

   # estimate variances
   data.lme		<- data.frame(y=as.vector(y), xo=as.vector(xo), xu=as.vector(xu), group=as.vector(group))
   
   # Use lm() to estimate fixed-effects for use as starting values in mixed-effects model

   temp.lm <- lm( y ~ -1 + xo + xu, data = data.lme )
   
   ## Use lmer() rather than lme() 
   
   temp		<- lmer(y ~ -1 + xo + xu + (1+xo+xu|group), data=data.lme, REML = TRUE,
                        start = c( xo = temp.lm$coefficients[[1]], xu = temp.lm$coefficients[[2]] ),
                        control = list(maxIter = 3000) )
   
   muO.hat		<- fixef(temp)[1]
   muU.hat		<- fixef(temp)[2]
   # Get variance for Intercept, xo, xu and Residuals
   var.hat		<- as.double(rbind(diag(VarCorr(temp)$group)[[1]],
                                   diag(VarCorr(temp)$group)[[2]],
								   diag(VarCorr(temp)$group)[[3]],
				                   attr(VarCorr(temp),"sc")[[1]]^2))
   llh		<- logLik(temp)[[1]]

   print(round(llh,3))
   print(round(sqrt(var.hat),3))
   flush.console()

   return( list(alpha.hat=alpha.hat, pi.hat=pi.hat, lambda.hat=lambda.hat,
                muO.hat=muO.hat, muU.hat=muU.hat, 
			    tau2.hat=var.hat[1], psi2.hat=var.hat[2], xi2.hat=var.hat[3], 
				sigma2.hat=var.hat[4], llh=llh) )
}

