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
\examples{
# Segmentate tracks.
# Create test data
chr <- character()
pos <- numeric()

for(i in 1:nrow(GRCh38)){
  chr <- c(chr,as.character(rep(GRCh38[i,"chr"],100)))
  pos <- c(pos,sample(GRCh38[i,"start"]:GRCh38[i,"end"],100))
}

value <- round(rexp(length(pos)),2)

track <- data.frame(chr,pos,pos+1,NA,value)
segments <- segmentation(track, 50000)


}
