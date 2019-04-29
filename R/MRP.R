MRP <- function(
  Z,
  method = c("exact", "mcmc", "approx"),
  error = 10^(-3),
  nit = NULL
) UseMethod("MRP", Z)

MRP.incidence <- function(
  Z,
  method = c("exact", "mcmc", "approx"),
  error = 10^(-3),
  nit = NULL
) {
  prfnames <- rownames(Z)
  n <- length(prfnames)
  if (sum(Z) == n*(n+1)/2) {
    class(Z) <- "matrix"
    return(Z*1)
  }
  res <- Z - diag(n)
  if (is.null(nit))
    nit <- floor(n^4*log(n)^2+n^3*n*log(error^(-1)))
  res <- switch(method[1],
                exact = exact_rank_prob(res)$relative.rank,
                mcmc = mcmc_rank_prob(res, rp = nit)$relative.rank,
                approx = approx_rank_relative(res)
  )
  rownames(res) <- colnames(res) <- prfnames
  
  return(res + diag(n))
}