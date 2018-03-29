\name{parsec2igraph}
\alias{parsec2igraph}
\alias{parsec2igraph.cover}
\alias{parsec2igraph.incidence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a partial order to an object of the package igraph
}
\description{
This function is introduced in the package in order to clarify the relation of an object of class \code{cover} and the adjacency matrix of the corresponding graph describing the cover relation.

Objects of class \code{cover} are boolean matrices indicating if the element of the row \emph{is covered by} the element of the corresponding column. This definition is the transpose of the adjacency matrix of a graph describing the cover relation. This characteristic is not changed in order to guarantee the compatibility of the package with the scripts created with the previous versions of the package.

Moreover, this function allows to use the package \code{\link{igraph}} to plot and analyse the partial order.
}
\usage{
parsec2igraph(p, ...)
\method{parsec2igraph}{cover}(p, ...)
\method{parsec2igraph}{incidence}(p, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
an object of class \code{cover} or \code{incidence}.
}
  \item{\dots}{
additional arguments of the function \code{\link{vertices}}.
}
}
\value{
The function returns an object of class \code{igraph} that represents the directed graph describing the cover relation.

The function add to the graph a layout generated with the function \code{\link{vertices}} in order to plot the graph of the cover relation similarly to an Hasse diagram.
}
\references{
Csardi G, Nepusz T: The igraph software package for complex network research,
  InterJournal, Complex Systems 1695. 2006. http://igraph.org
}
\author{
Arcagni, A.
}

\seealso{
\code{\link{igraph}}, \code{\link{vertices}}
}
\examples{
example(merge.wprof)
poset <- getzeta(distribution)
incidence2cover(poset)

G <- parsec2igraph(poset, noise = TRUE)
get.adjacency(G)
# tkplot(G, vertex.size = distribution$freq, vertex.color = "white")

G <- parsec2igraph(poset, noise = 10)
# tkplot(G, vertex.size = distribution$freq, vertex.color = "white")
}