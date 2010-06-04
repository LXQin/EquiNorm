affy.norm.Estep <-
function(data.y.simple, data.x, theta.hat, P){
   alpha.hat	<- theta.hat$alpha.hat
   pi.hat		<- theta.hat$pi.hat
   lamda.hat	<- theta.hat$lamda.hat
   muO.hat		<- theta.hat$muO.hat
   muU.hat		<- theta.hat$muU.hat
   tau2.hat		<- theta.hat$tau2.hat
   phi2.hat		<- theta.hat$phi2.hat
   xi2.hat		<- theta.hat$xi2.hat
   sigma2.hat	<- theta.hat$sigma2.hat

   # compute residuals after subtracting array effects
   data.y.simple	<- compute.resid.ctd(data.y.simple, alpha.hat)

   # compute density for o(ver-expression)=1 and u(nder-expression)=1
   dens.ND		<- compute.dens.ND(data.y.simple, sigma2.hat, tau2.hat)
   dens.Ov		<- compute.dens.D(data.y.simple, data.x, sigma2.hat, tau2.hat, phi2.hat, muO.hat)
   dens.Un		<- compute.dens.D(data.y.simple, data.x, sigma2.hat, tau2.hat, xi2.hat, muU.hat)
   temp		<- 0 - apply(cbind(dens.ND, dens.Ov, dens.Un), 1, max)
   dens.ND		<- dens.ND + temp
   dens.Ov		<- dens.Ov + temp
   dens.Un		<- dens.Un + temp

   # compute o.hat and u.hat
   temp		<- (1-pi.hat)*exp(dens.ND)+pi.hat*lamda.hat*exp(dens.Ov)+pi.hat*(1-lamda.hat)*exp(dens.Un)
   o.hat 		<- pi.hat*lamda.hat*exp(dens.Ov)/temp
   u.hat		<- pi.hat*(1-lamda.hat)*exp(dens.Un)/temp
   o.hat		<- as.integer(o.hat>u.hat & o.hat>(1-o.hat-u.hat))
   u.hat		<- as.integer(u.hat>o.hat & u.hat>(1-o.hat-u.hat))

print(table(o.hat, u.hat))

   list(o.hat=o.hat, u.hat=u.hat)
}

