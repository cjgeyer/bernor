
 tol <- 1e-6

 library(bernor)
 load("salam-old.RData")
 attach(salam)
 
 set.seed(42)

 beta <- rnorm(ncol(x))
 sigma <- rgamma(length(unique(i)), 5, 5)
 b <- rnorm(ncol(z))

 out <- bernor(y[ , 1], beta, b, sigma, x, z, i)
 print(out)

 my.bernor <- function(y, beta, b, sigma, x, z, i) {
     eta <- x %*% beta + z %*% (sigma[i] * b)
     p <- 1 / (1 + exp(- eta))
     sum(dbinom(y, 1, p, log = TRUE)) + sum(dnorm(b, log = TRUE))
 }

 my.value <- my.bernor(y[ , 1], beta, b, sigma, x, z, i)
 print(my.value)
 all.equal(out$value, my.value, tolerance = tol)

 nparm <- length(beta) + length(sigma)
 epsilon <- 1e-8

 my.gradient <- rep(0, nparm)
 for (j in 1:nparm) {
     beta.eps <- beta
     sigma.eps <- sigma
     if (j <= length(beta)) {
         beta.eps[j] <- beta[j] + epsilon
     } else {
         sigma.eps[j - length(beta)] <- sigma[j - length(beta)] + epsilon
     }
     out.eps <- bernor(y[ , 1], beta.eps, b, sigma.eps, x, z, i)
     my.gradient[j] <- (out.eps$value - out$value) / epsilon
 }
 print(my.gradient)

 out <- bernor(y[ , 1], beta, b, sigma, x, z, i, deriv = 2)
 print(out)
 all.equal(out$gradient, my.gradient, tolerance = tol)
 
 my.hessian <- matrix(0, nparm, nparm)
 for (j in 1:nparm) {
     beta.eps <- beta
     sigma.eps <- sigma
     if (j <= length(beta)) {
         beta.eps[j] <- beta[j] + epsilon
     } else {
         sigma.eps[j - length(beta)] <- sigma[j - length(beta)] + epsilon
     }
     out.eps <- bernor(y[ , 1], beta.eps, b, sigma.eps, x, z, i, deriv = 1)
     my.hessian[ , j] <- (out.eps$gradient - out$gradient) / epsilon
 }
 print(my.hessian)

 all.equal(out$hessian, my.hessian, tolerance = tol)
 
