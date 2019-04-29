MRPlex <- function(profiles, selection = NULL) {
  stopifnot(class(profiles) == "wprof")
  prf <- profiles$profiles
  rm(profiles)
  k <- ncol(prf)
  Plex <- Vectorize(function(x, y) {
    if (x == y)
      return(1)
    x <- prf[x,]
    y <- prf[y,]
    p <- sum(x < y)
    q <- sum(x == y)
    if (q == k)
      return(1)
    s <- 0:q
    return(
      p/k*sum(factorial(q)/factorial(q-s)*factorial(k-s-1)/factorial(k-1))
    )
  })
  if (is.null(selection))
    selection <- rownames(prf)
  res <- outer(selection, selection, Plex)
  dimnames(res) <- list(selection, selection)
  return(res)
}