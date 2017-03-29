\name{getAssemblyFromFasta}
\alias{getAssemblyFromFasta}
\title{Create an assembly from a FASTA file for its use as parameter of \code{genomebrowser} or \code{genomemap}.}
\author{
David Barrios and Carlos Prieto. Bioinformatics, University of Salamanca. See \url{http://d3gb.usal.es/}
}
\note{
FASTA format is a text-based format for representing either nucleotide sequences or peptide sequences, in which nucleotides or amino acids are represented using single-letter codes.
}
\usage{
getAssemblyFromFasta(fasta)
}
\arguments{
\item{fasta}{a "character" string representing the input Fasta file to be added in the genome browser.}
}
\description{
\code{getAssemblyFromFasta} create assemblies from a FASTA file for their use as parameter of \code{genomebrowser} or \code{genomemap}.
}
\value{
a data frame in BED format ready to use as an assembly in \code{genomebrowser} or \code{genomemap}.
}
\seealso{
  The \sQuote{D3GB} Website: \url{http://d3gb.usal.es}

  \code{\link{genomebrowser}}, \code{\link{genomemap}}.
}
\examples{
# Download fasta file
fasta <- tempfile()
download.file(paste0("ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/297/395/",
"GCF_000297395.2_ASM29739v2/GCF_000297395.2_ASM29739v2_genomic.fna.gz"),fasta)

# Assembly generation
getAssemblyFromFasta(fasta)
}
