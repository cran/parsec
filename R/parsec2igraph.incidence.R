parsec2igraph.incidence <- function(p, ...) {
	p <- incidence2cover.incidence(p)
	res <- parsec2igraph.cover(p, ...)
	return(res)
}