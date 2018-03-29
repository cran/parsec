# x: first wprof
# y: second wprof
# support: is y the support of x? if TRUE the function does not sum the frequencies of y
# all: same as merge.data.frame
merge.wprof <- function(x, y, support = FALSE, FUN = "+", all = TRUE, ...) {
	
	if (support)
		profiles <- y$profiles
	else
		profiles <- merge(x$profiles, y$profiles, all = all, ...)
	
	freq <- rep(0, nrow(profiles))
	
	for (i in 1:nrow(profiles)) {
		sel <- which(apply(x$profiles, 1, function(r) all(profiles[i,] == r)))
		if (length(sel) > 1)
			stop("multiple profiles in x")
		if (length(sel) == 1) {
			freq[i] <- freq[i] + x$freq[sel]
			rownames(profiles)[i] <- names(x$freq[sel])
		}
		if (!support) {
			sel <- which(apply(y$profiles, 1, function(r) all(profiles[i,] == r)))
			if (length(sel) > 1)
				stop("multiple profiles in y")
			if (length(sel) == 1) {
				freq[i] <- do.call(FUN, list(freq[i], y$freq[sel]))
				rownames(profiles)[i] <- names(y$freq[sel])
			}
		}
	}
	
	names(freq) <- rownames(profiles)
	result <- list(profiles = profiles, freq = freq)
	class(result) <- "wprof"
	
	return(result)
}