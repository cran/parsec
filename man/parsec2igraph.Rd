\name{parsec2igraph}
\alias{parsec2igraph}
\alias{parsec2igraph.cover}
\alias{parsec2igraph.incidence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converting a partial order to an object of the package \code{\link{igraph}}.
}
\description{

The function turns a cover matrix to an \code{\link{igraph}} object, so as to allow using the graphical power of \code{\link{igraph}} to plot Hasse diagrams. Objects of class \code{cover} are boolean matrices where element ij is equal to 1 if element i \emph{is covered by} element j. This makes the cover matrix the transpose of the adjacency matrix of a graph, describing the cover relation in \code{\link{igraph}}.


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
The function returns an object of class \code{igraph}, representing the directed graph defined by the cover relation.

The function adds to the graph a layout generated through function \code{\link{vertices}}, so as to plot the graph according to the conventions used for Hasse diagrams.
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