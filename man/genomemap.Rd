\name{genomemap}
\alias{genomemap}
\title{Create an interative genome map.}
\author{
David Barrios and Carlos Prieto. Bioinformatics, University of Salamanca. See \url{http://d3gb.usal.es/}
}
\value{
Object of class \code{genomebrowser}.
}
\usage{
genomemap(assembly, mapTrack = NULL)
}
\arguments{
\item{assembly}{a genome assembly data frame. D3GB provides human assemblies (NCBI36, GRCh37, GRCh38), human assemblies with cytobands (GRCh37.bands or GRCh38.bands), or methods for creating your own assembly from a FASTA file \code{\link{getAssemblyFromFasta}}.}
\item{mapTrack}{a data frame with values to represent on the genome map in BED format: chromosome, start, end, name, value.}
}
\description{
\code{genomemap} creates an interactive genome map.
}
\seealso{
  The \sQuote{D3GB} Website: \url{http://d3gb.usal.es}

  \code{\link{createAssembly}}, \code{\link{getAssemblyFromFasta}}, \code{\link{GRCh37.bands}}, \code{\link{GRCh38.bands}}.
}
\examples{
gm <- genomemap(GRCh38.bands)
}
