\name{gbk2genomebrowser}
\alias{gbk2genomebrowser}
\title{Generates an interactive genome browser.}
\author{
David Barrios and Carlos Prieto. Bioinformatics, University of Salamanca. See \url{http://d3gb.usal.es/}
}
\value{
The function creates a folder in the working directory of your computer with an HTML document (index.html) which contains the graph. This file can be directly opened with Firefox. If you want to open this file stored in your local machine with other Web browser, please visit the help section on the D3GB Web site (\url{http://d3gb.usal.es}).
}
\usage{
gbk2genomebrowser(gbkfile, server = FALSE, dir = "GenomeBrowser")
}
\arguments{
\item{gbkfile}{a "character" string representing the input GenBank file to be represented in the genome browser.}
\item{server}{a logical value to enable (\code{TRUE}) or disable (\code{FALSE}) the server mode. Server mode: designed to be shared as a website. Resulting folder should be added to the Apache applications directory and enable writting permissions. In this way the genome browser will be working as a web site. Local mode: the genome browser will be functional on your local machine. }
\item{dir}{a "character" string representing the directory where the graph will be saved.}
}
\description{
\code{gbk2genomebrowser} creates an interactive genome browser from a GenBank file.
}
\examples{
\dontrun{
# Download GenBank file
gbff <- tempfile()
download.file(paste0("ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/297/395/",
"GCF_000297395.2_ASM29739v2/GCF_000297395.2_ASM29739v2_genomic.gbff.gz"),gbff)

# Genome browser generation.
# It creates a genome browser ready to be viewed in Firefox.
# For a server version, ready to be shared with Apache as a Website, set the parameter server=True
gb <- gbk2genomebrowser(gbff, dir = "Micromonospora_gbk")
}
}
