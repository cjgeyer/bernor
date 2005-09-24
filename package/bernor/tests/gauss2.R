
 library(bernor)

 print(models())

 d <- 7
 sigma <- 0.8
 moo <- model("gauss", d, sigma)

 invisible(runif(1))
 save.seed <- .Random.seed

 x <- rmiss(moo)

 .Random.seed <- save.seed
 all.equal(x, rnorm(d, mean = 0, sd = sigma))

 doo <- dmiss(x, moo)

 all.equal(doo, sum(dnorm(x, mean = 0, sd = sigma, log = TRUE)))

