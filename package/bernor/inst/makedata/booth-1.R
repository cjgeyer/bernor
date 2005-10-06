
 #################### data ####################

 y <- as.matrix(read.table("mcculloch.txt"))
 dimnames(y) <- NULL
 y <- t(y)

 x <- as.matrix(seq(1, nrow(y)) / nrow(y))

 z <- as.matrix(rep(1, nrow(y)))

 i <- 1

 #################### "true" parameter values ####################

 mu0 <- 5
 sigma0 <- sqrt(1 / 2)
 theta0 <- c(mu0, sigma0)

 #################### dump data ####################

 booth <- list(y = y, x = x, z = z, i = i, mu0 = mu0, sigma0 = sigma0,
     theta0 = theta0)
 save(booth, file = "booth-1.RData", ascii = TRUE)

