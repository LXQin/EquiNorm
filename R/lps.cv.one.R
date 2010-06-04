lps.cv.one <-
function(train.data, train.label, test.data, test.label, gene.select, vt.G){

        mc      <- rep(0, length(vt.G))                         # misclassification error

        for(j in 1:length(vt.G)){
                # get the top genes
                train.data.select       <- train.data[gene.select[1:vt.G[j]],]
                test.data.select        <- cbind(test.data[gene.select[1:vt.G[j]],])
                if(vt.G[j]==1){
                 train.data.select<- rbind(train.data[gene.select[1:vt.G[j]],])
                 test.data.select       <- rbind(test.data[gene.select[1:vt.G[j]],])
                }

                # compute LPS = sum(t_g*X_g)
                weights         <- new.t.test.fast(train.data.select, train.label)$tt
                train.data.lps  <- weights %*% train.data.select
                test.data.lps   <- weights %*% test.data.select

                # train predictor
                class1          <- train.label==1
                n1                      <- sum( class1)
                n2                      <- sum(!class1)
                mu1                     <- mean(train.data.lps[ class1])
                mu2                     <- mean(train.data.lps[!class1])
                sd1                     <- sqrt((sum((train.data.lps[ class1])^2) - n1*mu1^2)/(n1-1))
                sd2                     <- sqrt((sum((train.data.lps[!class1])^2) - n2*mu2^2)/(n2-1))

                # predict test data
                f1                      <- dnorm(test.data.lps, mu1, sd1)
                f2                      <- dnorm(test.data.lps, mu2, sd2)
                pred.label              <- as.integer(f1/(f1+f2) > 0.5)

                mc[j]                   <- length(which(pred.label != test.label))
        }
        mc
}

