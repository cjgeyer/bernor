
 library(bernor, lib.loc = "../library")
 library(mcmc)

 data(salam)

 ##### global variables #####

 attach(salam) # contains x, y, z, i

 nparm <- ncol(x) + length(unique(i))

 ##### seeds #####

 set.seed(42)

 ##### functions #####

 h <- function(b) {
     b <- matrix(b, ncol = ncol(y))
     result <- 0
     for (j in 1:ncol(y))
         result <- result + bernor(y[ , j], mu, b[ , j], sigma, x, z, i)$value
     return(result)
 }

 g <- function(b) {
     b <- matrix(b, ncol = ncol(y))
     result <- rep(0, nparm)
     for (j in 1:ncol(y))
         result <- result + bernor(y[ , j], mu, b[ , j], sigma, x, z, i,
             deriv = 1)$gradient
     return(result)
 }

 ##### try 1 #####

 # from Booth and Hobert (1999)
 mu <- c(1.03, 0.32, -1.95, 0.99)
 sigma <- sqrt(c(1.40, 1.25))

 b <- rep(0, length(i) * ncol(y))
 out <- metrop(h, b, nbatch = 100, blen = 1000, outfun = g)
 print(out$time)
 print(out$accept)

 ##### try 2 #####

 out <- metrop(out, scale = 0.1)
 print(out$time)
 print(out$accept)
 print(apply(out$batch, 2, mean))
 print(apply(out$batch, 2, sd) / sqrt(out$nbatch))

 ##### try 3 #####

 out <- metrop(out, scale = 0.2)
 print(out$time)
 print(out$accept)
 print(apply(out$batch, 2, mean))
 print(apply(out$batch, 2, sd) / sqrt(out$nbatch))

 ##### try 4 #####

 out <- metrop(out, scale = 0.15)
 print(out$time)
 print(out$accept)
 print(apply(out$batch, 2, mean))
 print(apply(out$batch, 2, sd) / sqrt(out$nbatch))

 ##### try 5 #####

 out <- metrop(out, blen = 1e4)
 print(out$time)
 print(out$accept)
 print(apply(out$batch, 2, mean))
 print(apply(out$batch, 2, sd) / sqrt(out$nbatch))

 ##### dump data #####

 save.image(file = "mcmc1.RData")

 ##### machine info #####

 system("hostname")
 system("cat /proc/cpuinfo")

