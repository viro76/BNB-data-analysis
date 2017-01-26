#********************************
# Calculate quantiles


ma_data <- matrix(ncol = 50, nrow = 200)

for (i in 1:50) {
  a <- runif(1:200, min = 0, max = 100)
  ma_data[,i] <- a
}

ma_ecdf <- apply(ma_data, 2, ecdf)
x <- seq(from = 0, to = 1, by =0.1)
ma <- matrix(ncol = 50, nrow = length(x))

for (i in 1:length(x)) {    
  prob <- x[i]
  for (j in 1:length(ma_ecdf)){
    ma[i,j] <- quantile(ma_ecdf[[j]], probs = prob)
  }
}

# calculate the highflow, the median and the lowflows----
apply(ma, 2, quantile, prob=c(0.1,0.5,0.9, 0.95))



