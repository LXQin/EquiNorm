train.pam.bkup <-
function(data, label, vt.k=NULL, n.k=30, nfold=NULL, folds=NULL){
        m               <- ncol(data)
        data.pam        <- list(x=data, y=factor(label))
        pam.obj <- pamr.train(data.pam, threshold=vt.k, n.threshold=n.k)
        cl              <- pamr.cv(pam.obj, data.pam, nfold, folds)# leave-one-out CV
        mc              <- round(cl$error*m, 0)
        list(k=cl$size, mc=mc)
}

