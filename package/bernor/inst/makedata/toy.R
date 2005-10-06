
 RNGversion("1.9.1")
 set.seed(4242)

 mu0 <- 1
 sigma0 <- c(1, 1)
 theta0 <- c(mu0, sigma0)
 
 i <- scan("toyi.txt")
 z <- scan("toyz.txt")
 nrowz <- system("cat toyz.txt | wc -l", intern = TRUE)
 print(nrowz)
 nrowz <- as.numeric(nrowz)
 z <- matrix(z, nrow = nrowz, byrow = TRUE)
 x <- matrix(1, nrowz, 1)

 nobs <- 50

 y <- NULL
 for (blurfle in 1:nobs) {
     b <- rnorm(length(i))
     eta <- x %*% mu0 + z %*% (sigma0[i] * b)
     p <- 1 / (1 + exp(- eta))
     ysim <- as.numeric(runif(length(p)) < p)
     y <- cbind(y, ysim)
 }
 dimnames(y) <- NULL

 toy <- list(x = x, y = y, z = z, i = i, mu0 = mu0, sigma0 = sigma0,
     theta0 = theta0)

 save(toy, file = "toy.RData", ascii = TRUE)

