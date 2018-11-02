\name{latex}
\alias{latex}
\alias{latex.wprof}
\alias{latex.incidence}
\alias{latex.cover}
\title{
latex
}
\description{
The function returns the LaTeX code to create a tikz figure representing the Hasse diagram drawn from a set of profiles (\code{prof}), an incidence matrix (\code{Z}) or a cover matrix (\code{C}). The code can be copied and pasted into a \code{latex} file. The \code{latex} source requires the \code{tikz} package,
}
\usage{
latex(y, ...)
\method{latex}{wprof}(y, label = "", caption = "", scale = c(1, 1), ...)
\method{latex}{incidence}(y, label = "", caption = "", scale = c(1, 1), ...)
\method{latex}{cover}(y, label = "", caption = "", scale = c(1, 1), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
an object of S3 class \code{wprof}, an object of S3 class \code{incidence} or an object of S3 class \code{cover}.
}
  \item{label}{
the label of the LaTeX figure.
}
  \item{caption}{
the caption of the LaTeX figure.
}
  \item{scale}{
a vector of two elements to control the scale of the X-axis and the scale of the Y-axis in the LaTeX output.
}
  \item{\dots}{
any of above.
}
}
\examples{
prof <- var2prof(varlen = c(2, 3))
latex(prof, label="fg:hasse", caption="Hasse diagram", scale = c(2, 2))
}