
 # see pp. 439 ff. in McCullagh and Nelder (2nd ed)

 # our design.txt is Table 14.3 in McC & N
 # note, as pointed out by Yun Ju, column 1 is FEMALES,
 # rest of columns are MALES, not the other way round
 # as I had originally

 foo <- scan("design.txt", what = character(0))
 foo <- matrix(foo, nrow = 20, byrow = TRUE)

 females <- foo[ , 1]
 design <- foo[ , - 1]
 males <- unique(as.vector(design))

 setequal(males, females) ## should be TRUE
 # since TRUE can use females (which are in nice order) as index for both

 # our s1.txt is Table 14.4 in McC & N
 # our f1.txt is Table 14.5 in McC & N
 # our f2.txt is Table 14.6 in McC & N

 foo <- scan("s1.txt")
 s1 <- matrix(foo, nrow = 20, byrow = TRUE)
 foo <- scan("f1.txt")
 f1 <- matrix(foo, nrow = 20, byrow = TRUE)
 foo <- scan("f2.txt")
 f2 <- matrix(foo, nrow = 20, byrow = TRUE)

 # unlike before, combine all bernoullis into one long vector
 
 foo <- females[row(design)]
 bar <- as.vector(design)

 y <- cbind(as.vector(s1), as.vector(f1), as.vector(f2))

 pop.female <- substr(foo, 1, 1)
 pop.male <- substr(bar, 1, 1)
 mate <- paste(pop.female, pop.male, sep = "/")
 mate.u <- sort(unique(mate))

 x <- NULL
 for (qux in mate.u) x <- cbind(x, as.numeric(mate == qux))
 dimnames(x) <- list(NULL, mate.u)

 z <- NULL
 for (qux in females) z <- cbind(z, as.numeric(foo == qux))
 for (qux in females) z <- cbind(z, as.numeric(bar == qux))
 names.female <- paste("f", females, sep = "")
 names.male <- paste("m", females, sep = "")
 dimnames(z) <- list(NULL, c(names.female, names.male))

 i <- c(rep(1, length(females)), rep(2, length(females)))

 salam <- list(x = x, y = y, z = z, i = i)

 save(salam, file = "salam.RData", ascii = TRUE)

