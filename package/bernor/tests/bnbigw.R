
 tol <- 1e-6

 library(bernor)
 load("salam-old.RData")
 attach(salam)

 beta <- c(0.91, -3.01, -0.49, 3.54)
 sigma <- c(1.18, 0.98)

 moo <- model("gauss", length(i), 1)

 nmiss <- 100
 set.seed(42)
 W <- bnbigw(y, beta, sigma, nmiss, x, z, i, moo, nbatch = nmiss)
 print(W)

 nobs <- ncol(y)
 nparm <- length(beta) + length(sigma)

 my.bnbigw <- function(y, beta, sigma, nmiss, x, z, iv, model, nbatch) {
     nbatch <- as.integer(nbatch)
     nmiss <- as.integer(nmiss)
     blen <- nmiss %/% nbatch
     if (nmiss != nbatch * blen) {
         cat("nmiss =", nmiss, "\n")
         cat("nbatch =", nbatch, "\n")
         cat("blen =", blen, "\n")
         stop("oopsie")
     }
     W <- matrix(0, nparm, nparm)
     save.random.seed <- .Random.seed
     Shat.bmean <- rep(0, nparm)
     for (i in 1:nmiss) {
         Shat <- rep(0, nparm)
         b <- rmiss(model)
         for (j in 1:nobs) {
             foo <- bernor(y[ , j], beta, b, sigma, x, z, iv, deriv = 1)
             save.current.random.seed <- .Random.seed
             .Random.seed <<- save.random.seed
             bar <- bnmarg(y[ , j], beta, sigma, nmiss, x, z, iv, model,
                 deriv = 1, want.weights = TRUE)
             .Random.seed <<- save.current.random.seed
             piece <- (foo$gradient - bar$gradient) * nmiss * bar$weigh[i]
             Shat <- Shat + piece
         }
         Shat <- Shat / nobs
         Shat.bmean <- Shat.bmean + Shat
         if (i %% blen == 0) {
             Shat.bmean <- Shat.bmean / blen
             W <- W + outer(Shat.bmean, Shat.bmean)
             Shat.bmean <- Shat.bmean * 0
         }
     }
     W <- W * blen / nbatch
     return(W)
 }

 set.seed(42)
 myW <- my.bnbigw(y, beta, sigma, nmiss, x, z, i, moo, nbatch = nmiss)
 print(myW)

 all.equal(W, myW, tolerance = tol)

 ############### now with batch means ###############

 set.seed(43)
 W <- bnbigw(y, beta, sigma, nmiss, x, z, i, moo, nbatch = 10)
 print(W)
 set.seed(43)
 myW <- my.bnbigw(y, beta, sigma, nmiss, x, z, i, moo, nbatch = 10)
 print(myW)

 all.equal(W, myW, tolerance = tol)

