equi.gene.norm <-
function(data.y, data.x, pi.start=0.1, lambda.start=0.9){
	# starting value
	theta.hat	<- equi.gene.norm.start(data.y, data.x, pi.start, lambda.start)
	# EM algorithm
	est.hat.new <- equi.gene.norm.EM(data.y, data.x, theta.hat)

	# Save output to external file
	save( est.hat.new, file = "est.hat.Rdata" )
	return( est.hat.new )
}

