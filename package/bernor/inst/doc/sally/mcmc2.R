
 load("mcmc1.RData")
 ls(all.names = TRUE)

 library(bernor)
 library(mcmc)

 data(salam)
 attach(salam) # contains x, y, z, i

 ##### functions #####

 gh <- function(b) {
     b <- matrix(b, ncol = ncol(y))
     result <- rep(0, nparm + nparm * (nparm + 1) / 2)
     for (j in 1:ncol(y)) {
         out <- bernor(y[ , j], mu, b[ , j], sigma, x, z, i, deriv = 2)
         foo <- out$gradient
         bar <- out$hessian
         qux <- c(foo, bar[row(bar) >= col(bar)])
         result <- result + qux
     }
     return(result)
 }

 ##### try 1 #####

 out <- metrop(out, outfun = gh)
 print(out$time)
 print(out$accept)
 grad.hat <- apply(out$batch[ , 1:nparm], 2, mean)
 grad.mcse <- apply(out$batch[ , 1:nparm], 2, sd) / sqrt(out$nbatch)
 hess.hat <- apply(out$batch[ , -(1:nparm)], 2, mean)
 hess.mcse <- apply(out$batch[ , -(1:nparm)], 2, sd) / sqrt(out$nbatch)
 bar <- matrix(0, nparm, nparm)
 bar[row(bar) >= col(bar)] <- hess.hat
 diag(bar) <- diag(bar) / 2
 bar <- bar + t(bar)
 print(grad.hat)
 print(grad.mcse)
 print(bar)

 grad.mcv <- t(out$batch[ , 1:nparm]) %*% out$batch[ , 1:nparm] / out$nbatch^2
 bigS <- solve(bar) %*% grad.mcv %*% solve(bar)
 print(bigS, digits = 4)
 theta.hat.mcse <- sqrt(diag(bigS))
 print(theta.hat.mcse)

 ##### dump data #####

 save(file = "mcmc2.RData")

 ##### machine info #####

 system("hostname")
 system("cat /proc/cpuinfo")

