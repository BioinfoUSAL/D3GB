\name{gbk2genomebrowser}
\alias{gbk2genomebrowser}
\title{Generates an interactive genome browser.}
\author{
David Barrios and Carlos Prieto. Bioinformatics, University of Salamanca. See \url{http://d3gb.usal.es/}
}
\value{
Object of class \code{genomebrowser}.
}
\usage{
gbk2genomebrowser(gbkfile, namelabel = NULL)
}
\arguments{
\item{gbkfile}{a "character" string representing the input GenBank file to be represented in the genome browser.}
\item{namelabel}{a "character" string giving the GenBank field to be represented as feature name in the genome browser. By default the function will search for "locus_tag" or "gene" fields.}
}
\description{
\code{gbk2genomebrowser} creates an interactive genome browser from a GenBank file.
}
\examples{
\donttest{
# Download GenBank file
gbff <- tempfile()
download.file(paste0("https://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/297/395/",
"GCF_000297395.2_ASM29739v2/GCF_000297395.2_ASM29739v2_genomic.gbff.gz"),gbff)

# Genome browser generation.
gb <- gbk2genomebrowser(gbff)

plot(gb)
}
}
