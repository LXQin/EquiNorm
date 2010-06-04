new.t.test.fast <-
function(data, label, f=0) {
        class1  <- label==1
        n1              <- sum( class1)
        n2              <- sum(!class1)
        m1              <- apply(data[, class1], 1, mean)
        m2              <- apply(data[,!class1], 1, mean)
        v1              <- apply((data[, class1])^2, 1, sum) - n1*(m1)^2
        v2              <- apply((data[,!class1])^2, 1, sum) - n2*(m2)^2
        s12             <- v1/(n1-1)
        s22             <- v2/(n2-1)
        ss              <- sqrt(s12/n1 + s22/n2)
        tt              <- (m2-m1)/(ss+f)
        df              <- (s12/n1+s22/n2)^2/((s12/n1)^2/(n1+1)+(s22/n2)^2/(n2+1)) - 2
        df              <- round(df, 0)
        pp              <- pt(-abs(tt), df) * 2
        list(pp=pp, tt=tt)
}

