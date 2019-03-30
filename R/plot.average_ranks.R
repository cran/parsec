plot.average_ranks <- function(x, range.first = TRUE, range.col = "black", range.lty = 1, range.lwd = 1, type = "p", ylim = c(nrow(x), 1), xlab = "", ylab = "Average rank", pch = c(16, 3, 3), col = "black", cex = c(1, 1, 1), ...) {
	x <- x[order(x$avrg, decreasing = TRUE),]
  xaxt <- "n"
	matplot(x[,c("avrg", "inf", "sup")], ylim = ylim, ylab = ylab, xaxt = xaxt, xlab = xlab, cex = cex, pch = pch, col = col, type = type, ...)
	axis(1, at = 1:nrow(x), labels = row.names(x))
	segments(x0 = 1:nrow(x), y0 = x$inf, y1 = x$sup, col = range.col, lty = range.lty, lwd = range.lwd)
	if(range.first)
		points(x[, "avrg"], pch = pch[1], cex = cex[1], col = col[1], type = type[1])
}