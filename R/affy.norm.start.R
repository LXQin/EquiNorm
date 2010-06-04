affy.norm.start <-
function(data.y, data.x, pi.start){
   # variance components
   sigma2.hat 	<- 0.5
   tau2.hat		<- 1
   phi2.hat		<- 1
   xi2.hat		<- 1

   # differential expression
   muO.hat		<- 3
   muU.hat		<- -2

   # gene proportions
   pi.hat 		<- pi.start
   lamda.hat	<- 0.6

   # rank-invariant genes
   data.y.simple	<- apply(data.y, 1:2, mean)
   G			<- dim(data.y.simple)[1]
   gene.rank	<- apply(data.y.simple, 2, rank)
   gene.rank.var	<- apply(gene.rank, 1, var)
   cutoff		<- sort(gene.rank.var)[G*(1-pi.start)]
   idx.gene		<- gene.rank.var < cutoff
   alpha.hat	<- apply(data.y.simple[idx.gene,], 2, median)
   #alpha.hat	<- apply(data.y, 2, median)				# overall median

   theta.hat	<- list(alpha.hat=alpha.hat, pi.hat=pi.hat, lamda.hat=lamda.hat, muO.hat=muO.hat, muU.hat=muU.hat,
					tau2.hat=tau2.hat, phi2.hat=phi2.hat, xi2.hat=xi2.hat, sigma2.hat=sigma2.hat)
   theta.hat
}

