rank_stability.FODposet <- function(x, selection = 1:length(x$covers), coverage_probability = 0.9, error = 10^(-5), ...) {
	
  colv <- 1 - coverage_probability
  
	sel_names <- names(x$covers[selection])
	selection <- which(names(x$covers) %in% sel_names)
	
	ranks <- lapply(selection, function(s) {
		if (nrow(x$covers[[s]]) == 1) {
			print(paste("alpha =", round(x$posets.ind$alpha[s], 4), "- only one element in the poset"))
			res <- 1
			names(res) <- rownames(x$covers[[s]])
			return(data.frame(avg = res, min = res, max = res))
		}
		if (all(!x$covers[[s]])) {
			print(paste("alpha =", round(x$posets.ind$alpha[s], 4), "- the poset is an antichain"))
			n <- nrow(x$covers[[s]])
			res <- rep((n+1)/2, n)
			names(res) <- rownames(x$covers[[s]])
			return(data.frame(avg = res, min = res, max = res))
		}
		print(paste("alpha =", round(x$posets.ind$alpha[s], 4)))
		thr <- which(rowSums(x$covers[[s]]) == 0)
		ZETA <- cover2incidence(x$covers[[s]])
		
		res <- idn(zeta = ZETA, threshold = thr, error = error)$rank_dist
		
		# uniformo gli average rank dei profili equivalenti
		eq <- equivalences(ZETA)
		
		distr <- by(res, eq, colMeans)
		distr <- t(sapply(eq, function(i) distr[[i]]))
		rownames(distr) <- names(eq)
		
		nv <- nrow(x$covers[[s]])
		
		avgr <- distr %*% nv:1
		
		cumdistr <- apply(distr, 1, cumsum)
		
		minr <- nv - apply(cumdistr, 2, function(x) min(nv, which(x >= colv/2 & x != 0))) + 1
		maxr <- nv - apply(cumdistr, 2, function(x) min(nv, which(x >= 1 - colv/2))) + 1
		
		return(data.frame(avg = avgr, min = minr, max = maxr))
	})
	
	ranking <- lapply(ranks, function(r) rank(r$avg))

	avrgranks <- t(sapply(1:length(selection), function(i) t(ranks[[i]]$avg) %*% x$eqv.classes[[selection[i]]]))
	colnames(avrgranks) <- colnames(x$eqv.classes[[selection[1]]])
	rownames(avrgranks) <- sel_names
	
	minranks <- t(sapply(1:length(selection), function(i) t(ranks[[i]]$min) %*% x$eqv.classes[[selection[i]]]))
	colnames(minranks) <- colnames(x$eqv.classes[[selection[1]]])
	rownames(minranks) <- sel_names
	
	maxranks <- t(sapply(1:length(selection), function(i) t(ranks[[i]]$max) %*% x$eqv.classes[[selection[i]]]))
	colnames(maxranks) <- colnames(x$eqv.classes[[selection[1]]])
	rownames(maxranks) <- sel_names
	
	ranking <- t(sapply(1:length(selection), function(i) t(ranking[[i]]) %*% x$eqv.classes[[selection[i]]]))
	colnames(ranking) <- colnames(x$eqv.classes[[selection[1]]])
	rownames(ranking) <- sel_names
	
	res <- list(alpha = x$posets.ind$alpha[selection], average_ranks = avrgranks, lower_ranks = minranks, upper_ranks = maxranks, ranking = ranking, resolution = sapply(x$covers, nrow))
	class(res) <- "rank_stability"
	
	return(res)
	
}