
 library(bernor)
 data(flu)

 ##### first do the old way with no weights #####

 beta <- c(-2.064, -2.231, -2.420, -2.341)
 delta <- c(-0.561, 1.181, -0.613)

 i <- rep(1:nrow(flu), times = flu$nobs)
 y <- rbind(flu$y1[i], flu$y2[i], flu$y3[i], flu$y4[i])
 rm(i)

 x <- diag(4)
 z <- rbind(c(1, 1, 1, 0, 0, 0), c(1, 1, 0, 1, 0,
     0), c(1, 1, 0, 0, 1, 0), c(1, -1, 0, 0, 0, 1))
 idx <- c(1, 2, 3, 3, 3, 3)

 set.seed(42)
 .save.Random.seed <- .Random.seed
 moo <- model("gaussian", length(idx), 1)
 nmiss <- 1000

 .Random.seed <- .save.Random.seed
 out <- bnlogl(y, beta, delta, nmiss, x, z, idx, moo, deriv = 3)
 lapply(out, round, digits = 3)

 ##### now the new way #####

 y <- rbind(flu$y1, flu$y2, flu$y3, flu$y4)
 weigh <- flu$nobs
 y <- y[ , weigh > 0]
 weigh <- weigh[weigh > 0]

 # print(y)
 # print(weigh)

 .Random.seed <- .save.Random.seed
 nout <- bnlogl(y, beta, delta, nmiss, x, z, idx, moo, deriv = 3,
     weigh = weigh)

 all.equal(out$value, nout$value)
 all.equal(out$gradient, nout$gradient)
 all.equal(out$hessian, nout$hessian)
 all.equal(out$bigv, nout$bigv)

