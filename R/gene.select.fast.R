gene.select.fast <-
function(data, label, G, f=0){
        pp              <- new.t.test.fast(data, label)$pp
        gene.select     <- order(pp)[1:G]
        gene.select
}

