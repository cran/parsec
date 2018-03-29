pop2prof <- function(y, labtype = c("profiles", "progressive"), sep="", weights = rep(1, nrow(y))) {

	pop_names <- apply(y, 1, function(x) paste(x, collapse = sep))
	freq <- by(weights, pop_names, sum)
	prf_names <- names(freq)
	freq <- as.vector(freq)
	# names(freq) <- prf_names
	
	sel <- sapply(prf_names, function(x) which(pop_names == x)[1])
	profiles <- y[sel,]
	rownames(profiles) <- names(freq) <- prf_names
	m <- nrow(profiles)
	if (labtype[1] == "progressive") {
		labels <- sprintf(paste("P%0", ceiling(log(m, 10)), "i", sep = ""), 1:m)
		rownames(profiles) <- names(freq) <- labels
	}
	res <- list(profiles = as.data.frame(profiles), freq = freq)
	class(res) <- "wprof"
	return(res)
}