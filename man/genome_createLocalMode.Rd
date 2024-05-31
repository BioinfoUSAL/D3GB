\name{genome_createLocalMode}
\alias{genome_createLocalMode}
\title{Generates an interactive genome browser.}
\author{
David Barrios and Carlos Prieto. Bioinformatics, University of Salamanca. See \url{http://d3gb.usal.es/}
}
\value{
The function creates a folder in the working directory of your computer with an HTML document (index.html) which contains the graph. This file can be directly opened with your browser.
}
\usage{
genome_createLocalMode(gb, dir="GenomeBrowser")
}
\arguments{
\item{gb}{a genome browser object produced by the function \code{genomebrowser}.}
\item{dir}{a "character" string representing the directory where the graph will be saved.}
}
\description{
\code{genome_createLocalMode} creates an interactive genome browser to open directly in your browser.
}
\seealso{
  The \sQuote{D3GB} Website: \url{http://d3gb.usal.es}

  \code{\link{genomebrowser}}, \code{\link{createAssembly}}, \code{\link{getAssemblyFromFasta}}, \code{\link{GRCh37.bands}}, \code{\link{GRCh38.bands}}, \code{\link{genome_addGFF}}, \code{\link{genome_addSequence}}, \code{\link{genome_addTrack}}, \code{\link{genome_addVCF}}.
}
\examples{
# Genome browser generation.
gb <- genomebrowser(GRCh38.bands)
# It creates a genome browser ready to be viewed in your browser.
genome_createLocalMode(gb,dir=tempfile())
}
