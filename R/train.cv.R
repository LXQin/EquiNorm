train.cv <-
function(data, label, method, vt.k, vt.G, K.cv){
        # split the sample into K parts
        m               <- ncol(data)
        m.b             <- balancedFolds(label, K.cv)

        #K.o            <- ceiling(m/K.cv)
        #m.b            <- c(sample(m,m), rep(0, K.o*K.cv-m))
        #m.b            <- matrix(m.b, ncol=K.o, byrow=TRUE)

        # K-fold CV
        mc.knn  <- matrix(0, nrow=length(vt.k), ncol=length(vt.G))
        mc.lda  <- rep(0, length(vt.G))
        mc.lps  <- rep(0, length(vt.G))
        for(i in 1:K.cv){
                temp    <- train.cv.one(m.b[[i]], data, label, method, vt.k, vt.G)
                mc.knn<- mc.knn + temp$mc.knn
                mc.lda<- mc.lda + temp$mc.lda
                mc.lps<- mc.lps + temp$mc.lps

        print(paste(i, "th fold"))

        }
        list(mc.knn=mc.knn, mc.lda=mc.lda, mc.lps=mc.lps)
}

