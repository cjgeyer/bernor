
 library(bernor)

 print(models())

 d <- 7
 scale <- 1.2
 df <- 3.5
 moo <- model("stu", d, c(scale, df))

 invisible(runif(1))

 save.seed <- .Random.seed
 x <- rmiss(moo)

 .Random.seed <- save.seed
 x.too <- scale * rt(d, df)
 all.equal(x, x.too)

 doo <- dmiss(x, moo)
 doo.too <- sum(dt(x / scale, df = df, log = TRUE)) - d * log(scale)
 all.equal(doo, doo.too)

