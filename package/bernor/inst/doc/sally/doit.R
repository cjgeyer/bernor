
 library(bernor, lib.loc = "../library")

 data(salam)

 ##### global variables #####

 attach(salam) # contains x, y, z, i

 nparm <- ncol(x) + length(unique(i))
 nfix <- ncol(x)

 moo <- model("gaussian", length(i), 1.0)

 ##### seeds #####

 set.seed(42)
 .save.Random.seed <- .Random.seed

 ##### functions #####

 objfun <- function (theta) {
     if (!is.numeric(theta)) stop("objfun: theta not numeric")
     if (length(theta) != nparm) stop("objfun: theta wrong length")
     mu <- theta[seq(1, nfix)]
     sigma <- theta[- seq(1, nfix)]
     .Random.seed <<- .save.Random.seed
     bnlogl(y, mu, sigma, nmiss, x, z, i, moo)$value
 }

 objgrd <- function (theta) {
     if (!is.numeric(theta)) stop("objfun: theta not numeric")
     if (length(theta) != nparm) stop("objfun: theta wrong length")
     mu <- theta[seq(1, nfix)]
     sigma <- theta[- seq(1, nfix)]
     .Random.seed <<- .save.Random.seed
     bnlogl(y, mu, sigma, nmiss, x, z, i, moo, deriv = 1)$gradient
 }

 ##### try 1 #####

 nmiss <- 1e2

 theta.start <- rep(0, nparm)
 names(theta.start) <- c(dimnames(x)[[2]],
     paste("sigma", c("f", "m"), sep = "_"))
 lower <- rep(0, nparm)
 lower[1:ncol(x)] <- (- Inf)

 trust <- 1
 lowert <- pmax(lower, theta.start - trust)
 uppert <- theta.start + trust

 control <- list(fnscale = -10)
 out <- optim(theta.start, objfun, objgrd, method = "L-BFGS-B",
         lower = lowert, upper = uppert, control = control) 

 print(out)

 ##### try 2 #####

 nmiss <- 1e4

 theta.start <- out$par
 control <- list(fnscale = signif(out$value, 1))

 lowert <- pmax(lower, theta.start - trust)
 uppert <- theta.start + trust

 out <- optim(theta.start, objfun, objgrd, method = "L-BFGS-B",
         lower = lowert, upper = uppert, control = control) 

 print(out)

 ##### try 3 #####

 nmiss <- 1e7

 theta.start <- out$par
 control <- list(fnscale = signif(out$value, 1))

 lowert <- pmax(lower, theta.start - trust)
 uppert <- theta.start + trust

 out <- optim(theta.start, objfun, objgrd, method = "L-BFGS-B",
         lower = lowert, upper = uppert, control = control) 

 print(out)

 ##### wrap up #####

 theta.hat <- out$par
 mu.hat <- theta.hat[1:nfix]
 sigma.hat <- theta.hat[- (1:nfix)]
 .Random.seed <<- .save.Random.seed
 lout <- bnlogl(y, mu.hat, sigma.hat, nmiss, x, z, i, moo, deriv = 2)

 ##### dump data #####

 save(nmiss, out, lout, theta.hat, .save.Random.seed, file = "doit.RData")

 ##### machine info #####

 system("hostname")
 system("cat /proc/cpuinfo")

