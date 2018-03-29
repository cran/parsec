plot.rank_stability <- function(x, which = 1:4, legend = TRUE, legend.x = "bottomleft", legend.y = NULL, legend.bg = "white", grid = TRUE, grid.lty = 2, grid.col = rgb(0, 0, 0, 1/7), grid.lwd = 1, y_axis = "reversed", ask = dev.interactive(), type = "l", col = gray(1:ncol(x$ranking)/ncol(x$ranking)/1.3), lwd = 3, lty = 1, ...) {
	
	if (nrow(x$ranking)>1) {
		selposet <- which.max(
			sapply(1:nrow(x$average_ranks), function(i) {
				mean(abs(outer(x$average_ranks[i,], x$average_ranks[i,], "-")))*x$resolution[i]
			}))[1]
		x$average_ranks <- x$average_ranks[, order(x$ranking[selposet,])]
		x$ranking <- x$ranking[, order(x$ranking[selposet,])]
	}
	
	if(1 %in% which) {
		xlab <- ""
		ylab <- "Average rank"
		add <- FALSE
		if (y_axis == "reversed") {
			ylim <- c(max(x$average_ranks), 1)
		} else {
			ylim <- NULL
		}
		if (grid) {
			matplot(x$average_ranks, type = "n", ylim = ylim, xlab = xlab, ylab = ylab, ...)
			abline(v = 1:length(x$alpha), lty = grid.lty, col = grid.col, lwd = grid.lwd)
			add <- TRUE
		}
		matplot(x$average_ranks, ylim = ylim, add = add, type = type, xlab = xlab, ylab = ylab, col = col, lwd = lwd, lty = lty, ...)
		if (legend) {
			legend(x = legend.x, y = legend.y, legend = colnames(x$ranking), col = col, lty = lty, lwd = lwd, bg = legend.bg)
		}
		if (ask) {
			oask <- devAskNewPage(TRUE)
			on.exit(devAskNewPage(oask))
		}
	}
	
	if(2 %in% which) {
		xlab <- ""
		ylab <- "Ranking"
		add <- FALSE
		if (y_axis == "reversed") {
			ylim <- c(max(x$ranking), 1)
		} else {
			ylim <- NULL
		}
		if (grid) {
			matplot(x$ranking, type = "n", ylim = ylim, xlab = xlab, ylab = ylab, ...)
			abline(v = 1:length(x$alpha), lty = grid.lty, col = grid.col, lwd = grid.lwd)
			add <- TRUE
		}
		matplot(x$ranking, ylim = ylim, add = add, type = type, xlab = xlab, ylab = ylab, col = col, lwd = lwd, lty = lty, ...)
		if (legend) {
			legend(x = legend.x, y = legend.y, legend = colnames(x$ranking), col = col, lty = lty, lwd = lwd, bg = legend.bg)
		}
		if (ask) {
			oask <- devAskNewPage(TRUE)
			on.exit(devAskNewPage(oask))
		}
	}
	
	if(3 %in% which) {
		xlab <- "Alpha"
		ylab <- "Average rank"
		add <- FALSE
		if (y_axis == "reversed") {
			ylim <- c(max(x$average_ranks), 1)
		} else {
			ylim <- NULL
		}
		if (grid) {
			matplot(x$alpha, x$average_ranks, type = "n", ylim = ylim, xlab = xlab, ylab = ylab, ...)
			abline(v = x$alpha, lty = grid.lty, col = grid.col, lwd = grid.lwd)
			add <- TRUE
		}
		matplot(x$alpha, x$average_ranks, ylim = ylim, add = add, type = type, xlab = xlab, ylab = ylab, col = col, lwd = lwd, lty = lty, ...)
		if (legend) {
			legend(x = legend.x, y = legend.y, legend = colnames(x$ranking), col = col, lty = lty, lwd = lwd, bg = legend.bg)
		}
		if (ask) {
			oask <- devAskNewPage(TRUE)
			on.exit(devAskNewPage(oask))
		}
	}
	
	if(4 %in% which) {
		xlab <- "Alpha"
		ylab <- "Ranking"
		add <- FALSE
		if (y_axis == "reversed") {
			ylim <- c(max(x$ranking), 1)
		} else {
			ylim <- NULL
		}
		if (grid) {
			matplot(x$alpha, x$ranking, type = "n", ylim = ylim, xlab = xlab, ylab = ylab, ...)
			abline(v = x$alpha, lty = grid.lty, col = grid.col, lwd = grid.lwd)
			add <- TRUE
		}
		matplot(x$alpha, x$ranking, ylim = ylim, add = add, type = type, xlab = xlab, ylab = ylab, col = col, lwd = lwd, lty = lty, ...)
		if (legend) {
			legend(x = legend.x, y = legend.y, legend = colnames(x$ranking), col = col, lty = lty, lwd = lwd, bg = legend.bg)
		}
	}
}