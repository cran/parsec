parsec2igraph.cover <- function(p, ...) {
	g <- graph_from_adjacency_matrix(t(p))
	g$layout <- as.matrix(-vertices(p, ...))
	return(g)
}