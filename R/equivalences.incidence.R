equivalences.incidence <- function(x) {
	
	prf <- row.names(x)
	
	if (length(prf) == 1) {
		res <- 1
		names(res) <- prf
		return(res)
	}
	
	up <- sapply(prf, function(y) upset(x, y))
	diag(up) <- FALSE

	dw <- sapply(prf, function(y) downset(x, y))
	diag(dw) <- FALSE
	
	conf <- as.data.frame(t(rbind(up, dw))) # metto tutto insieme
	
	conf <- t(unique(t(apply(conf[duplicated(conf),], 1, function(z) {
		apply(conf, 1, function(y)  all(z == y))
	}))))
	conf <- as.vector(conf %*% 1:ncol(conf))
	
	if (length(conf) == 0)
		conf <- rep(0, length(prf))
	
	conf[conf == 0] <- max(conf)+1:sum(conf == 0)
	conf <- as.factor(paste("C", conf, sep = ""))
	names(conf) <- prf
	
	return(conf)
}