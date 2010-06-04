train.pam <-
function(data, label, vt.k=NULL, n.k=30, nfold=NULL, folds=NULL, B.cv=10, seed=10){
        m               <- ncol(data)
        data.pam        <- list(x=data, y=factor(label))
        pam.obj <- pamr.train(data.pam, threshold=vt.k, n.threshold=n.k)
        mc              <- rep(0, n.k)
        for(b in 1:B.cv){
                set.seed(seed+b-1)
                mc.b    <- pamr.cv(pam.obj, data.pam, nfold, folds)
                mc      <- mc + round(mc.b$error*m, 0)/B.cv
        }
        list(threshold=pam.obj$threshold, size.gene=pam.obj$size, mc=mc)
}

