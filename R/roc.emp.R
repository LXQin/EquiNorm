roc.emp <-
function(s, d){
	us 		<- unique(s)
	alpha		<- beta <- NULL
	for(i in 1.:length(us)) {
		alpha[i]	<- sum(s[d == 0.] >= us[i])
		beta[i]	<- sum(s[d == 1.] >= us[i])
	}
	n0		<- length(s[d==0.])
	n1		<- length(s[d==1.])
	alpha 	<- c(0., alpha[order(beta)]/n0)
	beta 		<- c(0., sort(beta)/n1)
	x 		<- sort(alpha)
	y 		<- beta[order(alpha)]

	wx		<- wilcox.test(s ~ d, conf.int=TRUE)$stat
	auc		<- 1-wx/(n0*n1)
	q1		<- auc/(2-auc)
	q2		<- 2*(auc^2)/(1+auc)
	auc.se	<- sqrt(auc*(1-auc)+(n1-1)*(q1-auc^2)+(n0-1)*(q2-auc^2))/sqrt(n0*n1)

	list(x=x, y=y, auc=auc, auc.se=auc.se)
}

