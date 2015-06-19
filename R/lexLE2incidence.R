lexLE2incidence <- function(lst,
    varmod = lapply(as.list(varlen), function(lst) 1:lst),
    varlen = sapply(varmod, length)
)
    UseMethod("lexLE2incidence")

lexLE2incidence.default <- function(lst,
        varmod = lapply(as.list(varlen), function(lst) 1:lst),
        varlen = sapply(varmod, length)
    )
{
    m <- prod(varlen)
    res <- expand.grid(varmod[lst])[m:1,]
    res <- res[,names(varlen)]
    nam <- apply(res, 1, function(r) paste(r, collapse = ""))
    res <- matrix(FALSE, m, m, dimnames = list(nam, nam))
    res[lower.tri(res, diag = TRUE)] <- TRUE
    class(res) <- "incidence"
    return(res)
}

lexLE2incidence.list  <- function(lst,
    varmod = lapply(as.list(varlen), function(x) 1:x),
    varlen = sapply(varmod, length)
) lapply(lst, function(x) lexLE2incidence(x, varmod, varlen))