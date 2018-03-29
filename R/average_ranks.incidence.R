average_ranks.incidence <- function(x, level = 0.9, error = 10^(-5), ...) {
	# poset
	ZETA <- x
	n <- nrow(ZETA)
	thr <- which(rowSums(incidence2cover(ZETA)) == 0)
	es <- idn(zeta=ZETA, threshold = thr, error=error)
	
	# uniformo gli average rank dei profili equivalenti
	RNK <- es$rank_dist
	eq <- equivalences(ZETA)
	RNK <- by(RNK, eq, colMeans)
	RNK <- t(sapply(eq, function(i) RNK[[i]]))
	rownames(RNK) <- names(eq)
	
	CONF <- function(DIST, LEV)
	{
		PROB <- 1
		x <- DIST
		while(PROB>=LEV)
		{
			y <- which(x>0)
			fl <- c(min(y), max(y))
			# meet<-fl[which((x[fl]==min(x[fl[1]], x[fl[2]]))==TRUE)]
			meet <- fl[which.min(x[fl])]
			x[meet] <- 0
			PROB<-sum(x)
		}
		x[meet] <- DIST[meet]
		PROB <- sum(x)
		c(min(which(x>0)), max(which(x>0)), PROB)
	}
	
	AVRG <- apply(RNK, 1, function(x) x%*%(1:n)) #Average rank
	MAX <- sapply(rownames(RNK), function(x) n-sum(upset(ZETA, x))+1)
	MIN <- sapply(rownames(RNK), function(x) sum(downset(ZETA, x)))
	RNG <- MAX - MIN #Range of position in the ranking
	LEVELS <- as.data.frame(t(apply(RNK, 1, function(x) CONF(x, level)))) #Call to CONF and computation of rank intervals comprisin probability = PROB
	colnames(LEVELS) <- c("inf", "sup", "prob") #(INF<=Ranking position<= SUP) >= PROB
	LEVELS[,1:2] <- n - LEVELS[,1:2] + 1
	DF <- data.frame(avrg = n - AVRG + 1, LEVELS, min = n - MAX + 1, max = n - MIN + 1, range = RNG)
	DF <- DF[order(DF$avrg),]
	class(DF) <- c("average_ranks", class(DF))
	return(DF)
}