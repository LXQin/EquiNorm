affy.norm.Mstep <-
function(data.y.simple, data.x, e.hat, theta.hat){
   # delist theta0.hat
   o.hat		<- e.hat$o.hat
   u.hat		<- e.hat$u.hat
   z.hat		<- o.hat + u.hat
   G			<- dim(data.y.simple)[1]
   n			<- dim(data.y.simple)[2]

   # frequency of each cluster
   pi.hat 		<- sum(z.hat)/G
   lamda.hat	<- sum(o.hat)/sum(z.hat)

   # estimate alpha.hat (array effect)
   alpha.hat	<- apply(data.y.simple[z.hat==0,], 2, mean)

   # xn=non-differential-expression, xo=over-expression, xu=under-expression
   y					<- compute.resid.ctd(data.y.simple, alpha.hat)
   group				<- t(apply(rbind(1:G), 2, rep, each=n))
   xo					<- matrix(0, nrow=G, ncol=n)
   xo[o.hat==1, data.x==1]	<- 1
   xu					<- matrix(0, nrow=G, ncol=n)
   xu[u.hat==1, data.x==1]	<- 1

   # estimate variances
   data.lme		<- data.frame(y=as.vector(y), xo=as.vector(xo), xu=as.vector(xu), group=as.vector(group))
   temp		<- lme(y ~ -1+xo+xu, data=data.lme, random = ~ 1+xo+xu|group, 
					control=list(maxIter=200, msMaxIter=100, 
						 opt="optim", optimMethod="L-BFGS-B", lower=c(0,-Inf), upper=c(Inf,0)))
   muO.hat		<- fixed.effects(temp)[1]
   muU.hat		<- fixed.effects(temp)[2]
   var.hat		<- as.double(VarCorr(temp)[,1])
   llh		<- temp$logLik

print(round(llh,3))
print(round(sqrt(var.hat),3))

   list(alpha.hat=alpha.hat, pi.hat=pi.hat, lamda.hat=lamda.hat, muO.hat=muO.hat, muU.hat=muU.hat, 
			tau2.hat=var.hat[1], phi2.hat=var.hat[2], xi2.hat=var.hat[3], sigma2.hat=var.hat[4], llh=llh)
}

