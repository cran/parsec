MRP <- function(Z, method = c("exact", "mcmc", "approx")) UseMethod("MRP", Z)
MRP.incidence <- function(Z, method = c("exact", "mcmc", "approx")) {
  prfnames <- rownames(Z)
  n <- length(prfnames)
  if (sum(Z) == n*(n+1)/2) {
    class(Z) <- "matrix"
    return(Z*1)
  }
  res <- Z - diag(n)
  res <- switch(method[1],
                exact = exact_rank_prob(res)$relative.rank,
                mcmc = mcmc_rank_prob(res, rp = nrow(res)^3)$relative.rank,
                approx = approx_rank_relative(res)
  )
  rownames(res) <- colnames(res) <- prfnames
  
  return(res + diag(n))
}