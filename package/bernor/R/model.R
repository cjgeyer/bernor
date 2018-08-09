model <- function(name, hyper, parm = double(0)) {

    if (! is.character(name)) stop("name not character")
    if (! is.numeric(hyper)) stop("hyper not numeric")
    if (! is.numeric(parm)) stop("parm not numeric")

    if (length(name) != 1) stop("name not scalar")
    name <- match.arg(name, models())
    imodel <- match(name, models()) - 1
    out <- .C(C_i1miss,
        model = as.integer(imodel),
        nhyper = integer(1))
    nhyper <- out$nhyper

    if (as.integer(hyper) != hyper) stop("hyper not integer")
    if (length(hyper) != nhyper) stop("hyper wrong length for model")
    out <- .C(C_i2miss,
        model = as.integer(imodel),
        hyper = as.integer(hyper),
        nparm = integer(1),
        nstate = integer(1))
    nparm <- out$nparm
    nstate <- out$nstate

    if (length(parm) != nparm) stop("parm wrong length for model and hyper")

    structure(list(name = name, hyper = hyper, parm = parm), class = "model")
}
