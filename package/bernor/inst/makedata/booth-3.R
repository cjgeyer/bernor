
 blather <- 0

 #################### special "Booth" functions ####################

 source("funs.R")

 #################### data ####################

 load("booth-2.RData")
 attach(booth)

 #################### fisher information ####################

 info <- fish(y, mu0, sigma0)
 print(info)

 #################### big W ####################

 W <- bigw(y, mu0, sigma0)
 print(W)

 #################### dump data ####################

 booth$info0 <- info
 booth$bigw0 <- W
 save(booth, file = "booth-3.RData", ascii = TRUE)

