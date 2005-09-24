
 tol <- 1e-6

 library(bernor)
 load("salam-old.RData")
 attach(salam)

 beta <- c(0.91, -3.01, -0.49, 3.54)
 sigma <- c(1.18, 0.98)

 moo <- model("gauss", length(i), 1)

 nmiss <- 100
 set.seed(42)
 out <- bnlogl(y, beta, sigma, nmiss, x, z, i, moo)
 print(out)

 my.bnlogl <- function(y, beta, sigma, nmiss, x, z, iv, model) {
     save.random.seed <- .Random.seed
     logf <- rep(NA, ncol(y))
     for (i in 1:ncol(y)) {
         .Random.seed <<- save.random.seed
         out <- bnmarg(y[ , i], beta, sigma, nmiss, x, z, iv, moo)
         logf[i] <- out$value
     }
     return(sum(logf))
 }

 set.seed(42)
 my.out <- my.bnlogl(y, beta, sigma, nmiss, x, z, i, moo)
 print(my.out)
 all.equal(out$value, my.out, tolerance = tol)

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
     set.seed(42)
     out.eps <- bnlogl(y, beta.eps, sigma.eps, nmiss, x, z, i, moo)
     my.gradient[j] <- (out.eps$value - out$value) / epsilon
 }
 print(my.gradient)

 set.seed(42)
 out <- bnlogl(y, beta, sigma, nmiss, x, z, i, moo, deriv = 3)
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
     set.seed(42)
     out.eps <- bnlogl(y, beta.eps, sigma.eps, nmiss, x, z, i, moo, deriv = 1)
     my.hessian[ , j] <- (out.eps$gradient - out$gradient) / epsilon
 }
 print(my.hessian)

 all.equal(out$hessian, my.hessian, tolerance = tol)
 
 my.bigv <- matrix(0, nparm, nparm)
 for (j in 1:ncol(y)) {
     set.seed(42)
     margout <- bnmarg(y[ , j], beta, sigma, nmiss, x, z, i, moo, deriv = 1)
     my.bigv <- my.bigv + outer(margout$gradient, margout$gradient)
 }
 my.bigv <- my.bigv / ncol(y)

 print(my.bigv)
 all.equal(out$bigv, my.bigv)

