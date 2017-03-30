\name{genome_addTrack}
\alias{genome_addTrack}
\title{Add tracks to \code{genomebrowser}.}
\author{
David Barrios and Carlos Prieto. Bioinformatics, University of Salamanca. See \url{http://d3gb.usal.es/}
}
\value{
The function add an input track to \code{genomebrowser}.
}
\usage{
genome_addTrack(gb, track, trackname = NULL, type = "gene", color = "#000", scale = NA)
}
\arguments{
\item{gb}{a genome browser object produced by the function \code{genomebrowser}.}

\item{track}{a "character" string giving the input BED file or a data frame with track information to be represented in the genome browser.}

\item{trackname}{a "character" string giving a name for the track.}

\item{type}{a "character" string with the type of track should be drawn. Possible types are: "gene", "exons", "domain", "value" or "score".}

\item{color}{a "character" string giving the color of the track.}

\item{scale}{a vector with two values which specifies the minimun and maximun limits in the representation of the "score" or "value" tracks. By default maximun and minimun scores are taken as the limits.}
}
\description{
\code{genome_addTrack} add tracks to \code{genomebrowser}.
}
\examples{
# Segmentate tracks.
\donttest{
genome_addTrack(gb, track)
}
}
