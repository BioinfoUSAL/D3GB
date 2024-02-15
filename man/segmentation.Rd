\name{segmentation}
\alias{segmentation}
\title{Function for segmentate tracks in BED format.}
\author{
David Barrios and Carlos Prieto. Bioinformatics, University of Salamanca. See \url{http://d3gb.usal.es/}
}
\note{
Input file is provided in BED format which is an universal format for genomic features.
}
\usage{
segmentation(track, cell)
}
\arguments{
\item{track}{a Data Frame with genomic features (in BED format).}

\item{cell}{a numeric value with the size of each segment.}
}
\description{
\code{segmentation} performs a segmentation of track data in bed format in order to enable its effective representation.
}
\seealso{
  The \sQuote{D3GB} Website: \url{http://d3gb.usal.es}

  \code{\link{genomebrowser}}, \code{\link{genomemap}}, \code{\link{genome_addTrack}}.
}
\value{
a segmented Data Frame in BED format.
}
