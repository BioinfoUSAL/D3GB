\name{genomebrowser}
\alias{genomebrowser}
\title{Generates an interactive genome browser.}
\author{
David Barrios and Carlos Prieto. Bioinformatics, University of Salamanca. See \url{http://d3gb.usal.es/}
}
\value{
The function creates a folder in the working directory of your computer with an HTML document (index.html) which contains the graph. This file can be directly opened with Firefox. If you want to open this file stored in your local machine with other Web browser, please visit the help section on the D3GB Web site (\url{http://d3gb.usal.es}).
}
\usage{
genomebrowser(assembly, tracks = NA, types = NA, colors = NA,
mapTrack = NA, server = FALSE, dir = "GenomeBrowser")
}
\arguments{
\item{tracks}{a list of data frames with track information that will be viewed in the genome browser. These data frames should have a BED format with the following columns: chromosome/scaffold, start, end, name, score, strand.}
\item{types}{a "character" vector with type of tracks that will be represented. It should be one of: "gene", "exons", "domain", "value" or "score".}
\item{colors}{a "character" vector with color used for the track representation.}
\item{mapTrack}{a data frame with values to represent on the genome map in BED format: chromosome, start, end, name, value.}
\item{assembly}{a genome assembly data frame. D3GB provides human assemblies (NCBI36, GRCh37, GRCh38), human assemblies with cytobands (GRCh37.bands or GRCh38.bands), or methods for creating your own assembly from a FASTA file \code{\link{getAssemblyFromFasta}}.}
\item{server}{a logical value to enable (\code{TRUE}) or disable (\code{FALSE}) the server mode. Server mode: designed to be shared as a website. Resulting folder should be added to the Apache applications directory and enable writting permissions. In this way the genome browser will be working as a web site. Local mode: the genome browser will be functional on your local machine. }
\item{dir}{a "character" string representing the directory where the graph will be saved.}
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
# It creates a genome browser ready to be viewed in Firefox.
# For a server version, ready to be shared with Apache as a Website, set the parameter server=True
gb <- genomebrowser(GRCh38.bands, dir="GRCh38_browser")
# Now you can add sequences, tracks, gff or vcf
}
