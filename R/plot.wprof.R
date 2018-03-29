plot.wprof <-
function(x,  shape = c("square", "circle", "equispaced"), noise = FALSE, ...) {
  Z <- getzeta(x)
  plot(Z, shape, noise, ...)
}
