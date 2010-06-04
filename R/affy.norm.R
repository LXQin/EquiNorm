affy.norm <-
function(data.y, data.x, pi.start=0.1){
	# starting value
	theta.hat	<- affy.norm.start(data.y, data.x, pi.start)
	# EM algorithm
	est.hat.new <- affy.norm.EM(data.y, data.x, theta.hat)
	est.hat.new
}

