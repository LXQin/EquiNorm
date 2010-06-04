train.knn.easy <-
function(data, label, vt.k=2:10){
        n.k             <- length(vt.k)
        mc              <- rep(0, n.k)
        for(i in 1:n.k){
                cl      <- knn.cv(t(data), label, vt.k[i])
                mc[i]   <- length(which(cl!=label))
                print(paste(i, ": ", mc[i], sep=""))
        }
        list(k=vt.k, mc=mc)
}

