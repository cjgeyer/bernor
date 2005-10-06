
 # y is vector (row of McCullough model)
 # z is vector (marginally iid standard normal)
 # return value and first derivative of log conditional of y | z
 # note: derivatives of log conditional are same as derivatives of log joint
 logf <- function(y, z, mu, sigma) {

     if (! is.numeric(y)) stop("y not numeric")
     if (! is.numeric(z)) stop("z not numeric")
     if (! is.numeric(mu)) stop("mu not numeric")
     if (! is.numeric(sigma)) stop("sigma not numeric")

     if (length(mu) != 1) stop("mu not scalar")
     if (length(sigma) != 1) stop("sigma not scalar")

     if (! all(is.element(y, 0:1))) stop("y not in {0, 1}")
     if (! (sigma >= 0.0)) stop("sigma not nonnegative")

     J <- length(y)
     x <- seq(1, J) / J
     eta <- outer(mu * x, sigma * z, "+")
     peta <- 1 / (1 + exp(- eta))
     qeta <- 1 / (1 + exp(eta))
     delta <- (- sweep(peta, 1, y))
     delta.x <- sweep(delta, 1, x, "*")
     delta.x <- apply(delta.x, 2, sum)
     delta.z <- apply(delta, 2, sum) * z
     qpeta <- 0 * peta
     qpeta[y == 1, ] <- log(peta[y == 1, ])
     qpeta[y == 0, ] <- log(qeta[y == 0, ])
     value <- apply(qpeta, 2, sum)
     gradient <- rbind(delta.x, delta.z)
     dimnames(gradient) <- NULL
     return(list(value = value, gradient = gradient))
 }

 # y is vector (row of McCullough model)
 # return value and first derivative of log marginal of y
 logfmarg <- function(y, mu, sigma) {
     foo <- function(z) exp(logf(y, z, mu, sigma)$value + dnorm(z, log = TRUE))
     L <- integrate(foo, lower = -Inf, upper = Inf)$value
     value <- log(L)
     foo <- function(z) {
         out <- logf(y, z, mu, sigma)
         return(out$gradient[1, ] * exp(out$value + dnorm(z, log = TRUE)))
     }
     g1 <- integrate(foo, lower = -Inf, upper = Inf)$value / L
     foo <- function(z) {
         out <- logf(y, z, mu, sigma)
         return(out$gradient[2, ] * exp(out$value + dnorm(z, log = TRUE)))
     }
     g2 <- integrate(foo, lower = -Inf, upper = Inf)$value / L
     return(list(value = value, gradient = c(g1, g2)))
 }

 # y is matrix (transpose of y for McCullough model)
 # return value and first derivative of log likelihood
 logl <- function(y, mu, sigma) {
     if (! is.matrix(y)) stop("y not matrix")
     nobs <- ncol(y)
     value <- 0
     gradient <- rep(0, 2)
     for (i in 1:nobs) {
         out <- logfmarg(y[ , i], mu, sigma)
         value <- value + out$value
         gradient <- gradient + out$gradient
     }
     return(list(value = value, gradient = gradient)) 
 }

 logl.old <- function(y, mu, sigma) {
     J <- nrow(y)
     nobs <- ncol(y)
     x <- seq(1, J) / J

     foo <- function(z, y) {
         if (prod(y * (1 - y)) != 0) stop("bogus y value")
         if (length(y) != J) stop("bogus y length")
         eta <- outer(mu * x, sigma * z, "+")
         peta <- 1 / (1 + exp(- eta))
         qeta <- 1 / (1 + exp(eta))
         peta[y == 0, ] <- 1
         qeta[y == 1, ] <- 1
         qpeta <- peta * qeta
         qpp <- apply(qpeta, 2, prod)
         return(qpp * dnorm(z))
     }

     bar <- function(z, y) {
         if (prod(y * (1 - y)) != 0) stop("bogus y value")
         if (length(y) != J) stop("bogus y length")
         eta <- outer(mu * x, sigma * z, "+")
         peta <- 1 / (1 + exp(- eta))
         qeta <- 1 / (1 + exp(eta))
         delta <- (- sweep(peta, 1, y))
         delta <- sweep(delta, 1, x, "*")
         delta <- apply(delta, 2, sum)
         peta[y == 0, ] <- 1
         qeta[y == 1, ] <- 1
         qpeta <- peta * qeta
         qpp <- apply(qpeta, 2, prod)
         return(qpp * delta * dnorm(z))
     }

     baz <- function(z, y) {
         if (prod(y * (1 - y)) != 0) stop("bogus y value")
         if (length(y) != J) stop("bogus y length")
         eta <- outer(mu * x, sigma * z, "+")
         peta <- 1 / (1 + exp(- eta))
         qeta <- 1 / (1 + exp(eta))
         delta <- (- sweep(peta, 1, y))
         delta <- apply(delta, 2, sum)
         peta[y == 0, ] <- 1
         qeta[y == 1, ] <- 1
         qpeta <- peta * qeta
         qpp <- apply(qpeta, 2, prod)
         return(qpp * delta * z * dnorm(z))
     }

     value <- 0
     grad.beta <- 0
     grad.sigma <- 0

     for (i in 1:nobs) {
         yi <- y[ , i]
         Li <- integrate(foo, lower = -Inf, upper = Inf, y = yi)$value
         value <- value + log(Li)
         Bi <- integrate(bar, lower = -Inf, upper = Inf, y = yi)$value
         grad.beta <- grad.beta + Bi / Li
         Si <- integrate(baz, lower = -Inf, upper = Inf, y = yi)$value
         grad.sigma <- grad.sigma + Si / Li
     }

     return(list(value = value, gradient = c(grad.beta, grad.sigma)))
 }

 objfun1 <- function(theta) {
     mu <- theta[1]
     sigma <- theta[2]
     return(- logl(y, mu, sigma)$value)
 }

 objfun2 <- function(theta) {
     mu <- theta[1]
     sigma <- theta[2]
     out <- logl(y, mu, sigma)
     result <- (- out$value)
     attr(result, "gradient") <- (- out$gradient)
     return(result)
 }

 fish <- function(y, mu, sigma) {
     J <- nrow(y)
     x <- seq(1, J) / J
     yi <- rep(0, J)

     result <- matrix(0, 2, 2)

     repeat {
         out <- logfmarg(yi, mu, sigma)
         result <- result + outer(out$gradient, out$gradient) * exp(out$value)

         carry <- 1
         for (j in 1:J) {
             yi[j] = yi[j] + carry
             if (yi[j] == 2) {
                 yi[j] <- 0
                 carry <- 1
             } else {
                 carry <- 0
             }
         }
         if (carry == 1) break
     }
     return(result)
 }

 fish.old <- function(y, mu, sigma) {
     J <- nrow(y)
     x <- seq(1, J) / J
     yi <- rep(0, J)

     result <- matrix(0, 2, 2)

     repeat {
         out <- logl.old(as.matrix(yi), mu, sigma)
         result <- result + outer(out$gradient, out$gradient) * exp(out$value)

         carry <- 1
         for (j in 1:J) {
             yi[j] = yi[j] + carry
             if (yi[j] == 2) {
                 yi[j] <- 0
                 carry <- 1
             } else {
                 carry <- 0
             }
         }
         if (carry == 1) break
     }
     return(result)
 }

 bigw <- function(y, mu, sigma) {
     J <- nrow(y)
     x <- seq(1, J) / J

     result <- matrix(0, 2, 2)

     ii <- c(1, 2, 1)
     jj <- c(1, 2, 2)
     for (k in 1:3) {
         i <- ii[k]
         j <- jj[k]

         ### calculate s_theta(z)[i] s_theta(z)[j] phi(z)
         foo <- function(z) {

             # cat("foo called with i =", i, ": j =", j, "\n")

             ### calculate s_theta(z)
             result <- rep(0, 2)
             yi <- rep(0, J)
             repeat {
                 out <- logf(yi, z, mu, sigma)
                 mout <- logfmarg(yi, mu, sigma)
                 result <- result + outer(mout$gradient, exp(out$value))

                 carry <- 1
                 for (jfoo in 1:J) {
                     yi[jfoo] = yi[jfoo] + carry
                     if (yi[jfoo] == 2) {
                         yi[jfoo] <- 0
                         carry <- 1
                     } else {
                         carry <- 0
                     }
                 }
                 if (carry == 1) break
             }
             ### result is now s_theta(z)
             result <- result[i, ] * result[j, ] * dnorm(z)

             # blurfle <- order(z)
             # blurfle <- cbind(z[blurfle], result[blurfle])
             # dimnames(blurfle) <- list(NULL, c("z", "foo(z)"))
             # print(blurfle)

             return(result)
         }

         result[i, j] <- integrate(foo, lower = -Inf, upper = Inf)$value
     }
     result[2, 1] <- result[1, 2]
     return(result)
 }
