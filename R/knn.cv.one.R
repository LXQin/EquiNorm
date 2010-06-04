knn.cv.one <-
function(train.data, train.label, test.data, test.label, gene.select, vt.k, vt.G){

        mc      <- matrix(0, nrow=length(vt.k), ncol=length(vt.G))              # misclassification error

        for(j in 1:length(vt.G)){
                train.data.select       <- train.data[gene.select[1:vt.G[j]],]
                test.data.select        <- test.data[ gene.select[1:vt.G[j]],]
                mc[,j]          <- apply(cbind(vt.k), MARGIN=1, FUN=new.knn, t(train.data.select), 
                                                   t(cbind(test.data.select)), cl=train.label, cl.test=test.label)
        }
        mc
}

