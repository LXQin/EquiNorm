new.t.test <-
function(x, label){
        class1  <- label==1
        pp              <- t.test(x[class1], x[!class1])[[3]]
        pp
}

