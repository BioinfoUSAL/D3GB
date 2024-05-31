\name{genome_addSequence}
\alias{genome_addSequence}
\title{Add genome sequence on Fasta format to \code{genomebrowser}.}
\author{
David Barrios and Carlos Prieto. Bioinformatics, University of Salamanca. See \url{http://d3gb.usal.es/}
}
\usage{
genome_addSequence(gb, fastafile)
}
\arguments{
\item{gb}{a genome browser object produced by the function \code{genomebrowser}.}

\item{fastafile}{a "character" string representing the input Fasta file to be added in the genome browser.}
}
\description{
\code{genome_addSequence} add FASTA sequences to \code{genomeviewer}.
}
\seealso{
  The \sQuote{D3GB} Website: \url{http://d3gb.usal.es}

  \code{\link{genomebrowser}}, \code{\link{genome_addGFF}}, \code{\link{genome_addTrack}}, \code{\link{genome_addVCF}}.
}
\value{
No return value, called for side effects
}
\examples{
\donttest{
# Download fasta file
fasta <- tempfile()
download.file(paste0("https://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/297/395/",
"GCF_000297395.2_ASM29739v2/GCF_000297395.2_ASM29739v2_genomic.fna.gz"),fasta)

# Genome browser generation.
gb <- genomebrowser(getAssemblyFromFasta(fasta))

genome_addSequence(gb,fasta)

plot(gb)
}
}
