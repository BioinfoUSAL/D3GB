\name{genome_addGFF}
\alias{genome_addGFF}
\title{Add track in a gff file to \code{genomebrowser}.}
\author{
David Barrios and Carlos Prieto. Bioinformatics, University of Salamanca. See \url{http://d3gb.usal.es/}
}
\usage{
genome_addGFF(gb, gfffile)
}
\arguments{
\item{gb}{a genome browser object produced by the function \code{genomebrowser}.}
\item{gfffile}{a "character" string representing the input gff file to be represented in the genome browser.}
}
\description{
\code{genome_addGFF} add track in a gff file to \code{genomebrowser}.
}
\seealso{
  The \sQuote{D3GB} Website: \url{http://d3gb.usal.es}

  \code{\link{genomebrowser}}, \code{\link{genome_addSequence}}, \code{\link{genome_addTrack}}, \code{\link{genome_addVCF}}.
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

# Download gff file and add to the genome browser
gff <- tempfile()
download.file(paste0("https://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/297/395/",
"GCF_000297395.2_ASM29739v2/GCF_000297395.2_ASM29739v2_genomic.gff.gz"),gff)

genome_addGFF(gb,gff)

plot(gb)
}
}
