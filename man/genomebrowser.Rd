\name{genomebrowser}
\alias{genomebrowser}
\title{Generates an interactive genome browser.}
\author{
David Barrios and Carlos Prieto. Bioinformatics, University of Salamanca. See \url{http://d3gb.usal.es/}
}
\value{
Object of class \code{genomebrowser}.
}
\usage{
genomebrowser(assembly, tracks = NULL, types = NA, colors = NA, mapTrack = NULL)
}
\arguments{
\item{tracks}{a list of data frames with track information that will be viewed in the genome browser. These data frames should have a BED format with the following columns: chromosome/scaffold, start, end, name, score, strand.}
\item{types}{a "character" vector with type of tracks that will be represented. It should be one of: "gene", "exons", "domain", "value" or "score".}
\item{colors}{a "character" vector with color used for the track representation.}
\item{mapTrack}{a data frame with values to represent on the genome map in BED format: chromosome, start, end, name, value.}
\item{assembly}{a genome assembly data frame. D3GB provides human assemblies (NCBI36, GRCh37, GRCh38), human assemblies with cytobands (GRCh37.bands or GRCh38.bands), or methods for creating your own assembly from a FASTA file \code{\link{getAssemblyFromFasta}}.}
}
\description{
\code{genomebrowser} creates an interactive genome browser.
}
\seealso{
  The \sQuote{D3GB} Website: \url{http://d3gb.usal.es}

  \code{\link{createAssembly}}, \code{\link{getAssemblyFromFasta}}, \code{\link{GRCh37.bands}}, \code{\link{GRCh38.bands}}, \code{\link{genome_addGFF}}, \code{\link{genome_addSequence}}, \code{\link{genome_addTrack}}, \code{\link{genome_addVCF}}.
}
\examples{
# Genome browser generation.
gb <- genomebrowser(GRCh38.bands)
# Now you can add sequences, tracks, gff or vcf
}
