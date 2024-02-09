
 tol <- 1e-6

 library(bernor)
 load("salam-old.RData")
 attach(salam)

 beta <- c(0.91, -3.01, -0.49, 3.54)
 sigma <- c(1.18, 0.98)

 moo <- model("gauss", length(i), 1)

 nmiss <- 100
 set.seed(42)
 out <- bnmarg(y[ , 1], beta, sigma, nmiss, x, z, i, moo, want.weights = TRUE)
## IGNORE_RDIFF_BEGIN
 print(out)
## IGNORE_RDIFF_END

 my.bnmarg <- function(y, beta, sigma, nmiss, x, z, iv, model) {
     logf <- rep(NA, nmiss)
     logh <- rep(NA, nmiss)
     for (i in 1:nmiss) {
         b <- rmiss(model)
         out <- bernor(y, beta, b, sigma, x, z, iv)
         logf[i] <- out$value
         logh[i] <- dmiss(b, model)
     }
     a <- max(logf - logh)
     value <- a + log(mean(exp(logf - logh - a)))
     w <- exp(logf - logh - a)
     w <- w / sum(w)
     return(list(value = value, weigh = w))
 }

 set.seed(42)
 my.out <- my.bnmarg(y[ , 1], beta, sigma, nmiss, x, z, i, moo)
## IGNORE_RDIFF_BEGIN
 print(my.out)
## IGNORE_RDIFF_END
 all.equal(out, my.out, tolerance = tol)

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
     out.eps <- bnmarg(y[ , 1], beta.eps, sigma.eps, nmiss, x, z, i, moo)
     my.gradient[j] <- (out.eps$value - out$value) / epsilon
 }
## IGNORE_RDIFF_BEGIN
 print(my.gradient)
## IGNORE_RDIFF_END

 set.seed(42)
 out <- bnmarg(y[ , 1], beta, sigma, nmiss, x, z, i, moo, deriv = 2)
## IGNORE_RDIFF_BEGIN
 print(out)
## IGNORE_RDIFF_END
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
     out.eps <- bnmarg(y[ , 1], beta.eps, sigma.eps, nmiss, x, z, i, moo,
         deriv = 1)
     my.hessian[ , j] <- (out.eps$gradient - out$gradient) / epsilon
 }
## IGNORE_RDIFF_BEGIN
 print(my.hessian)
## IGNORE_RDIFF_END

 all.equal(out$hessian, my.hessian, tolerance = tol)
 
