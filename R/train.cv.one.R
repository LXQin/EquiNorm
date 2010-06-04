train.cv.one <-
function(idx, data, label, method, vt.k, vt.G){

        train.data              <- data[,-idx]
        train.label             <- label[-idx]
        test.data               <- cbind(data[,idx])
        test.label              <- label[idx]

        # gene selection: index of the top significant genes ordered by t.test p.value
        gene.select             <- gene.select.fast(train.data, train.label, max(vt.G))

        result.knn              <- matrix(0, nrow=length(vt.k), ncol=length(vt.G))
        result.lda              <- rep(0, length(vt.G))
        result.lps              <- rep(0, length(vt.G))

        if(any(method=="knn"))
                result.knn      <- knn.cv.one(train.data, train.label, test.data, test.label, gene.select, vt.k, vt.G)
        if(any(method=="lda"))
                result.lda      <- lda.cv.one(train.data, train.label, test.data, test.label, gene.select, vt.G)
        if(any(method=="lps"))
                result.lps      <- lps.cv.one(train.data, train.label, test.data, test.label, gene.select, vt.G)

        list(mc.knn=result.knn, mc.lda=result.lda, mc.lps=result.lps)
}

