\name{genome_addVCF}
\alias{genome_addVCF}
\title{Add VCF tracks to \code{genomebrowser}.}
\author{
David Barrios and Carlos Prieto. Bioinformatics, University of Salamanca. See \url{http://d3gb.usal.es/}
}
\value{
The function add a VCF track to \code{genomebrowser}.
}
\usage{
genome_addVCF(gb, vcffile, trackname = NULL, show = NULL)
}
\arguments{
\item{gb}{a genome browser object produced by the function \code{genomebrowser}.}

\item{vcffile}{a "character" string representing the input VCF file to be represented in the genome browser.}

\item{trackname}{a "character" string giving a name for the track.}

\item{show}{a "character" vector giving the info features to display.}
}
\description{
\code{genome_addVCF} add VCF tracks to \code{genomebrowser}.
}
