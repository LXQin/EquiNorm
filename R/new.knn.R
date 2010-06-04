new.knn <-
function(k, train, test, cl, cl.test){
        cl.pred <- knn(train, test, cl, k, use.all = FALSE)
        length(which(cl.pred != cl.test))
}

