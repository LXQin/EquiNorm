equi.gene.norm.Mstep <-
function (data.y, data.x, e.hat, theta.hat, lemma.outdir, lemma.tol, 
    lemma.maxIts, lemma.plots) 
{
    Sample.names <- NULL
    control.counter <- 1
    case.counter <- 1
    for (i in 1:length(data.x)) {
        if (data.x[i] == 0) {
            Sample.names <- c(Sample.names, paste("Y1_", control.counter, 
                sep = ""))
            control.counter <- control.counter + 1
        }
        else {
            Sample.names <- c(Sample.names, paste("Y2_", case.counter, 
                sep = ""))
            case.counter <- case.counter + 1
        }
    }
    geneid <- matrix(1:dim(data.y)[[1]], nrow = dim(data.y)[[1]], 
        ncol = 1)
    genename <- matrix("", 1:dim(data.y)[[1]], nrow = dim(data.y)[[1]], 
        ncol = 1)
    data.y.lemma <- as.data.frame(cbind(geneid, genename, data.y))
    data.y.lemma[, 1] <- as.integer(as.character(data.y.lemma[, 
        1]))
    data.y.lemma[, 2] <- as.factor(data.y.lemma[, 2])
    for (j in 3:dim(data.y.lemma)[[2]]) {
        data.y.lemma[, j] <- as.numeric(as.character(data.y.lemma[, 
            j]))
    }
    names(data.y.lemma) <- c("geneid", "genename", Sample.names)
    lemma(data.y.lemma, saveascsv = TRUE, plots = lemma.plots, 
        tol = lemma.tol, maxIts = lemma.maxIts, outdir = lemma.outdir)
    working.directory <- getwd()
    setwd(lemma.outdir)
    load("AllData.RData")
    setwd(working.directory)
    u.hat <- NULL
    o.hat <- NULL
    for (i in 1:length(RRfdr0)) {
        predicted.group <- sample(x = c(0, 1, 2), size = 1, replace = FALSE, 
            prob = c(round(RRfdr0[i], digits = 6), round(RRfdr1[i], 
                digits = 6), round(RRfdr2[i], digits = 6)))
        if (predicted.group == 0) {
            u.hat[i] <- 0
            o.hat[i] <- 0
        }
        else if (predicted.group == 1) {
            u.hat[i] <- 1
            o.hat[i] <- 0
        }
        else {
            u.hat[i] <- 0
            o.hat[i] <- 1
        }
    }
    z.hat <- u.hat + o.hat
    return(list(z.hat = z.hat, u.hat = u.hat, o.hat = o.hat))
}
