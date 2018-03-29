average_ranks.cover <- function(x, level = 0.9, error = 10^(-5), ...) {
	x <- cover2incidence(x)
	average_ranks.incidence(x, level=level, error=error)
}