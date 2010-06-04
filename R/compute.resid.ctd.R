compute.resid.ctd <-
function(data.y.simple, alpha.hat){
	resid			<- t(t(data.y.simple) - alpha.hat)
	resid
}

