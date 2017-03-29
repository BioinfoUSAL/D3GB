\name{genomemap}
\alias{genomemap}
\title{Create an interative genome map.}
\author{
David Barrios and Carlos Prieto. Bioinformatics, University of Salamanca. See \url{http://d3gb.usal.es/}
}
\value{
The function creates a folder in your computer with an HTML document named index.html which contains the graph. This file can be directly opened with Firefox. If you want to open this file stored in your local machine with other Web browser, please visit the help section on the D3GB Web site (\url{http://d3gb.usal.es}).
}
\usage{
genomemap(assembly, mapTrack = NA, dir = "GenomeMap")
}
\arguments{
\item{assembly}{a genome assembly data frame. D3GB provides human assemblies (NCBI36, GRCh37, GRCh38), human assemblies with cytobands (GRCh37.bands or GRCh38.bands), or methods for creating your own assembly from a FASTA file \code{\link{getAssemblyFromFasta}}.}
\item{mapTrack}{a data frame with values to represent on the genome map in BED format: chromosome, start, end, name, value.}
\item{dir}{a "character" string representing the directory where the graph will be saved.}
}
\description{
\code{genomemap} creates an interactive genome map.
}
\seealso{
  The \sQuote{D3GB} Website: \url{http://d3gb.usal.es}

  \code{\link{createAssembly}}, \code{\link{getAssemblyFromFasta}}, \code{\link{GRCh37.bands}}, \code{\link{GRCh38.bands}}.
}
\examples{
genomemap(GRCh38.bands, dir="GRCh38_map")
}
