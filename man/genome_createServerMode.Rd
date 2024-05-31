\name{genome_createServerMode}
\alias{genome_createServerMode}
\title{Generates an interactive genome browser.}
\author{
David Barrios and Carlos Prieto. Bioinformatics, University of Salamanca. See \url{http://d3gb.usal.es/}
}
\value{
The function creates a website. Resulting folder should be added to the Apache applications directory. In this way the genome browser will be working as a web site.
}
\usage{
genome_createServerMode(gb, dir="GenomeBrowser")
}
\arguments{
\item{gb}{a genome browser object produced by the function \code{genomebrowser}.}
\item{dir}{a "character" string representing the directory where the graph will be saved.}
}
\description{
\code{genome_createServerMode} creates an interactive genome browser to be shared as a website.
}
\seealso{
  The \sQuote{D3GB} Website: \url{http://d3gb.usal.es}

  \code{\link{genomebrowser}}, \code{\link{createAssembly}}, \code{\link{getAssemblyFromFasta}}, \code{\link{GRCh37.bands}}, \code{\link{GRCh38.bands}}, \code{\link{genome_addGFF}}, \code{\link{genome_addSequence}}, \code{\link{genome_addTrack}}, \code{\link{genome_addVCF}}.
}
\examples{
# Genome browser generation.
gb <- genomebrowser(GRCh38.bands)
# It creates a website. You can add it to your Apache server.
genome_createServerMode(gb,dir=tempfile())
}
