rmiss <- function(model) {

    if (! inherits(model, "model")) stop("model not class \"model\"")

    imodel <- match(model$name, models()) - 1
    out <- .C(C_i1miss,
        model = as.integer(imodel),
        nhyper = integer(1))
    nhyper <- out$nhyper

    if (length(model$hyper) != nhyper) stop("hyper wrong length for model")
    out <- .C(C_i2miss,
        model = as.integer(imodel),
        hyper = as.integer(model$hyper),
        nparm = integer(1),
        nstate = integer(1))
    nparm <- out$nparm
    nstate <- out$nstate

    if (length(model$parm) != nparm)
        stop("parm wrong length for model and hyper")

    out <- .C(C_rmiss,
        model = as.integer(imodel),
        hyper = as.integer(model$hyper),
        parm = as.double(model$parm),
        result = double(nstate))
    return(out$result)
}
