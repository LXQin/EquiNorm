affy.norm.EM <-
function(data.y, data.x, theta.hat){
   # average probes
   data.y.simple	<- apply(data.y, 1:2, mean)
   P			<- dim(data.y)[3]

   llh 		<- -999999999990
   llh.old 		<- -999999999999
   while(llh-llh.old>0.01){
	llh.old	<- llh
	# E-step
	e.hat 	<- affy.norm.Estep(data.y.simple, data.x, theta.hat, P)
	# M-step
	theta.hat	<- affy.norm.Mstep(data.y.simple, data.x, e.hat, theta.hat)
	llh		<- theta.hat$llh
   } 
   list(theta.hat=theta.hat, e.hat=e.hat)
}

