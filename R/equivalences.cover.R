equivalences.cover <- function(x) {
	x <- cover2incidence.cover(x)
	equivalences.incidence(x)
}