transitiveClosure.incidence <-
function(m) {
    g <- incidence2cover(m)
    ct <- transitiveClosure.default(g)
    class(ct) <- "cover"
    cover2incidence(ct)
}
