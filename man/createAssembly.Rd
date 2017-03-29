\name{createAssembly}
\alias{createAssembly}
\title{Creates a genome assembly for \code{genomebrowser} or \code{genomemap}.}
\author{
David Barrios and Carlos Prieto. Bioinformatics, University of Salamanca. See \url{http://d3gb.usal.es/}
}
\usage{
createAssembly(name, size)
}
\arguments{
\item{name}{a vector with the chomosome or scaffold names.}

\item{size}{a vector with the chomosome or scaffold sizes.}
}
\description{
\code{createAssembly} create assemblies for their use as parameter of \code{genomebrowser} or \code{genomemap}.
}
\value{
a data frame in BED format to be used as an assembly in \code{genomebrowser} or \code{genomemap}.
}
\seealso{
  The \sQuote{D3GB} Website: \url{http://d3gb.usal.es}

  \code{\link{genomebrowser}}, \code{\link{genomemap}}.
}
\examples{
# A Micromonospora scaffolds' names and sizes
scaffolds <- c("NZ_HF570105.1", "NZ_HF570106.1", "NZ_HF570107.1", "NZ_HF570108.1")
sizes <- c(583, 1327, 241394, 7082520)

# Create an assembly
createAssembly(scaffolds, sizes)
}
