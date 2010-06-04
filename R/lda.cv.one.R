lda.cv.one <-
function(train.data, train.label, test.data, test.label, gene.select, vt.G){

        mc      <- rep(0, length(vt.G))                         # misclassification error

        for(j in 1:length(vt.G)){
                train.data.select       <- train.data[gene.select[1:vt.G[j]],]
                test.data.select        <- cbind(test.data[gene.select[1:vt.G[j]],])
                if(vt.G[j]==1){
                 train.data.select<- rbind(train.data[gene.select[1:vt.G[j]],])
                 test.data.select       <- rbind(test.data[gene.select[1:vt.G[j]],])
                }
                # perform LDA in train set of selected genes
                train.dataframe <- data.frame(t(train.data.select), class=train.label)
                test.dataframe  <- data.frame(t(test.data.select),  class=test.label)
                train.classifier        <- lda(class~., train.dataframe)
                pred.label              <- predict(train.classifier, test.dataframe)$class
                mc[j]                   <- length(which(pred.label != test.label))
        }
        mc
}

