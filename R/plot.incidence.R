plot.incidence <-
function(x, shape = c("square", "circle", "equispaced"), noise = FALSE, ...) {
  C <- incidence2cover(x)
  plot(C, shape, noise, ...)
}
