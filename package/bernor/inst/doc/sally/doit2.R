
 library(bernor, lib.loc = "../library")

 data(salam)

 ##### global variables #####

 attach(salam) # contains x, y, z, i

 nparm <- ncol(x) + length(unique(i))
 nfix <- ncol(x)

 moo <- model("gaussian", length(i), 1.0)

 ##### seeds #####

 load(file = "doit.RData")

 ##### wrap up #####

 mu.hat <- theta.hat[1:nfix]
 sigma.hat <- theta.hat[- (1:nfix)]

 .Random.seed <<- .save.Random.seed
 wout <- bnbigw(y, mu.hat, sigma.hat, nmiss, x, z, i, moo)

 ##### dump data #####

 save(wout, file = "doit2.RData")

 ##### machine info #####

 system("hostname")
 system("cat /proc/cpuinfo")

