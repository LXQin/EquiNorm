train.classifier <-
function(data, label, method=c("knn","lda","lps"), vt.k=2:10, vt.G=1:10, K.cv=10, B.cv=10, seed=10){
        # repeat K-fold CV B.cv times and then average
        mc.knn  <- matrix(0, nrow=length(vt.k), ncol=length(vt.G))
        mc.lda  <- rep(0, length(vt.G))
        mc.lps  <- rep(0, length(vt.G))
        for(b in 1:B.cv){
                set.seed(seed+b-1)
                mc.b    <- train.cv(data, label, method, vt.k, vt.G, K.cv)
                mc.knn<- mc.knn + mc.b$mc.knn/B.cv
                mc.lda<- mc.lda + mc.b$mc.lda/B.cv
                mc.lps<- mc.lps + mc.b$mc.lps/B.cv
        }
        if(!any(method=="knn")) mc.knn<- NULL
        if(!any(method=="lda")) mc.lda<- NULL
        if(!any(method=="lps")) mc.lps<- NULL

        n               <- ncol(data)
        list(k=vt.k, G=vt.G, mc.knn=mc.knn/n, mc.lda=mc.lda/n, mc.lps=mc.lps/n)
}

