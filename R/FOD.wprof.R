FOD.wprof <- FFOD.wprof <- function(profiles, distributions = as.data.frame(profiles$freq), lambda = do.call(getlambda, as.list(names(profiles$profiles))), ...) {

	prf <- profiles$profiles
	UNITS.FRQ <- as.data.frame(apply(distributions, 2, function(x) x/sum(x)))
	
	# MTCLOSURE
	# provides the min-transitive clousure of the matrix relation
	mtclosure<-function(relation)
	{
		N <- nrow(relation)
		
		if (ncol(relation) != N)
			stop("the matrix does not represent a fuzzy relation")
		
		for(i in 1:N)
		{
			for(j in 1:N)
			{
				for(k in 1:N)
				{
					relation[j,k]<-max(relation[j,k],min(relation[j,i], relation[i,k]))
				}
			}
		}
		
		return(relation)
	}
	
	if (any(rownames(prf) != rownames(UNITS.FRQ)))
		warning("Distributions rownames do not match profiles rownames, control if the order of the frequencies correspond to the profiles one.")
	
	prm <- LE(lambda) # permutazioni degli indicatori
	LEX.ORD <- lapply(prm, function(x) do.call(order, prf[,x]))
	
	POPS <- names(UNITS.FRQ)
	FOD.MATRIX <- outer(POPS, POPS,
						Vectorize(function(x, y) {
							if (x == y)
								return(1)
							else
								return(mean(
									sapply(LEX.ORD, function(ord) # sapply sostituisce unlist(lapply(...))
										UNITS.FRQ[[y]][ord]%*%cumsum(UNITS.FRQ[[x]][ord]) # funzione FOD.PROB
									))
								)
						})
	)
	rownames(FOD.MATRIX) <- colnames(FOD.MATRIX) <- POPS
	
	FOD.CLOSED<-mtclosure(FOD.MATRIX)
	
	APPROX.CELLS <- abs(FOD.MATRIX-FOD.CLOSED)
	APPROX.TOT<-sum(APPROX.CELLS)/sum(FOD.MATRIX)
	APPROX.TOT.CORR<-sum(APPROX.CELLS)/(sum(FOD.MATRIX)-nrow(FOD.MATRIX))
	
	alpha <- sort(unique(c(FOD.CLOSED)))
	quasi.order <- lapply(alpha, function(a) FOD.CLOSED >= a)
	names(quasi.order) <- alpha
	
	orders <- lapply(quasi.order, function(m) {
		n <- ncol(m)
		cases <- which(!duplicated(m))
		groups <- outer(cases, 1:n, Vectorize(function(i, j) identical(m[,i], m[,j])))
		solo <- rowSums(groups) == 1
		if (sum(solo) == 1)
			rownames(groups)[solo] <- rownames(m)[groups[solo,]]
		else
			rownames(groups)[solo] <- rownames(m)[apply(groups[solo,], 2, any)]
		rownames(groups)[!solo] <- paste0("G", 1:sum(!solo))
		colnames(groups) <- colnames(m)
		sel <- apply(groups, 1, function(x) min(which(x)))
		res <- as.matrix(m[sel, sel])
		rownames(res) <- colnames(res) <- names(sel)
		class(res) <- "incidence"
		return(list(equiv.classes = groups, incidence = res))
	})
	
	orders <- unlist(orders, recursive = FALSE)
	equiv.classes <- orders[1:length(alpha)*2-1]
	names(equiv.classes) <- alpha
	Z <- orders[1:length(alpha)*2]
	names(Z) <- alpha
	
	G <- lapply(Z, incidence2cover)
	
	cardinalities <- sapply(Z, nrow)
	comparabilities <- sapply(Z, function(z) sum(z)-nrow(z))
	incomparabilities <- cardinalities*(cardinalities-1)/2 - comparabilities
	
	indicators <- data.frame(alpha = alpha, cardinality = cardinalities, comparabilities = comparabilities, incomparabilities = incomparabilities, ci.ratio = comparabilities/incomparabilities)
	row.names(indicators) <- NULL
	
	results <- list("delta"=FOD.MATRIX, "mintr.delta"=FOD.CLOSED, "global.approx"=APPROX.TOT, "global.approx.corr"=APPROX.TOT.CORR, "cell.approx"=APPROX.CELLS, "posets.ind"=indicators, "eqv.classes"=equiv.classes, "covers"=G)
	class(results) <- "FODposet"
	
	return(results)
}