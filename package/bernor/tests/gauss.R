
 library(bernor)

 print(models())

 d <- 7
 moo <- model("gauss", d, 1)

 invisible(runif(1))
 save.seed <- .Random.seed

 x <- rmiss(moo)

 .Random.seed <- save.seed
 all.equal(x, rnorm(d))

 doo <- dmiss(x, moo)

 all.equal(doo, sum(dnorm(x, log = TRUE)))

