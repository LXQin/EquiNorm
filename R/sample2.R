sample2 <-
function(y, n){m<- length(y)
if(m<n)  l<- c(1:m, sample(m, n-m, replace=FALSE))
if(m>=n) l<- sample(m, n, replace=FALSE)
sort(y[l])
}

