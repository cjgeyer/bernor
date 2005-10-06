
 #################### data ####################

 load("booth-1.RData")
 attach(booth)

 #################### functions ####################

 source("funs.R")
 
 #################### do it ####################

 out <- nlm(objfun2, theta0)
 print(out)

 #################### do it again ####################

 out2 <- nlm(objfun2, out$estimate, fscale = abs(out$minimum))
 print(out2)

 #################### dump data ####################

 booth$theta.hat.exact <- out2$estimate
 save(booth, file = "booth-2.RData", ascii = TRUE)

