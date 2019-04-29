pop2prof <- function(y, labtype = c("profiles", "progressive", "rownames"), sep="", weights = rep(1, nrow(y))) {

	# pop_names <- apply(y, 1, function(x) paste(x, collapse = sep))
	# freq <- by(weights, pop_names, sum)
	# prf_names <- names(freq)
	# freq <- as.vector(freq)
	# # names(freq) <- prf_names
	# 
	# sel <- sapply(prf_names, function(x) which(pop_names == x)[1])
	# profiles <- y[sel,]
	# rownames(profiles) <- names(freq) <- prf_names
	# m <- nrow(profiles)
	# if (labtype[1] == "progressive") {
	# 	labels <- sprintf(paste("P%0", ceiling(log(m, 10)), "i", sep = ""), 1:m)
	# 	rownames(profiles) <- names(freq) <- labels
	# }
  y <- as.data.frame(y)
  sel <- apply(y, 1, paste0, collapse = "$%&")
  res <- by(1:nrow(y), sel, function(i) list(sum(weights[i]), rownames(y[i,])))
  profiles <- Reduce(rbind, lapply(1:length(res), function(j) y[res[[j]][[2]][1],]))
  res <- Reduce(rbind, res)
  res[,2] <- sapply(res[,2], paste0, collapse = "-")
  res <- as.data.frame(res)
  names(res) <- c("freq", "names")
  rownames(res) <- rownames(profiles) <- NULL
  freq <- unlist(res[,1])
  rownames(profiles) <- names(freq) <- paste0("P", 1:nrow(profiles))
  if (labtype[1] == "profiles")
    rownames(profiles) <- names(freq) <- apply(profiles, 1, paste0, collapse = sep)
  if (labtype[1] == "rownames")
    rownames(profiles) <- names(freq) <- res[,2]
	res <- list(profiles = as.data.frame(profiles), freq = freq)
	class(res) <- "wprof"
	return(res)
}