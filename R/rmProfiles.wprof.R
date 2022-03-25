rmProfiles.wprof <-
function(y, v, ...) {
    if (is.logical(v)) v <- which(v)
    if (is.character(v)) v <- which(rownames(y$profiles) %in% v)
    y$profiles <- y$profiles[-v,]
    y$freq <- y$freq[-v]
    return(y)
}
