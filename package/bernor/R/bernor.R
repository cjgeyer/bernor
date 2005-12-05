bernor <- function(y, beta, b, sigma, x, z, i, deriv = 0) {

    if (! is.numeric(y)) stop("y not numeric")
    if (! is.numeric(beta)) stop("beta not numeric")
    if (! is.numeric(b)) stop("b not numeric")
    if (! is.numeric(sigma)) stop("sigma not numeric")
    if (! is.numeric(x)) stop("x not numeric")
    if (! is.numeric(z)) stop("z not numeric")
    if (! is.numeric(i)) stop("i not numeric")
    if (! is.numeric(deriv)) stop("deriv not numeric")
    if (! is.matrix(x)) stop("x not matrix")
    if (! is.matrix(z)) stop("z not matrix")

    if (! all(is.element(y, 0:1))) stop("y not in 0, 1")
    ##### if (! all(sigma >= 0.0)) stop("sigma not nonnegative")
    if (! all(is.element(i, seq(along = sigma))))
        stop("! all(is.element(i, seq(along = sigma)))")
    if (length(i) != length(b))
        stop("length(i) != length(b)")
    if (length(deriv) != 1) stop("deriv not scalar")
    if (! is.element(deriv, 0:2)) stop("deriv not in 0, 1, 2")

    if (nrow(x) != length(y)) stop("nrow(x) != length(y)")
    if (nrow(z) != length(y)) stop("nrow(z) != length(y)")
    if (ncol(x) != length(beta)) stop("ncol(x) != length(beta)")
    if (ncol(z) != length(b)) stop("ncol(z) != length(b)")
    storage.mode(x) <- "double"
    storage.mode(z) <- "double"

    nparm <- length(beta) + length(sigma)

    out <- .C("bernor",
        leny = length(y),
        lenfix = length(beta),
        lenran = length(b),
        lenvar = length(sigma),
        ncolx = ncol(x),
        ncolz = ncol(z),
        y = as.integer(y),
        beta = as.double(beta),
        sigma = as.double(sigma),
        b = as.double(b),
        x = x,
        z = z,
        i = as.integer(i),
        value = double(1),
        grad = double(nparm),
        hess = matrix(as.double(0), nparm, nparm),
        deriv = as.integer(deriv),
        PACKAGE = "bernor")
    result <- list(value = out$value)
    if (deriv >= 1) result$gradient <- out$grad
    if (deriv == 2) result$hessian <- out$hess
    return(result)
}
