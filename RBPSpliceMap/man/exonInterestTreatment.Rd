\name{exonInterestTreatment}
\alias{exonInterestTreatment}

\title{exonInterestTreatment}

\description{
Given a GRanges object corresponding to exon coordinates, this function creates a new GRanges object with modified coordinates depending on the splice site and the padding chosen for the analysis.
}

\usage{
exonInterestTreatment(exonInterest, spSite, pE, pI)
}

\arguments{
  \item{exonInterest}{
GRanges object containing coordinates of interest (thess coordinates match with exon ones)
}
  \item{spSite}{
Splice Site of interest ("5SS" or "3SS")
}
  \item{pE}{
padding exon. Number of nucleotides required in the exon
}
  \item{pI}{
padding intron. Number of nucleotides required in the intron
}
}
\details{
Only strand = "+" or "-" are treated. If the input GRanges contains unspecified strands = "*", they are excluded.\cr
The returned GRanges contains the genomic coordinates of the region to be analysed around the chosen splice site.\cr
If an exon is at the beggining of the chromosome and, with the padding, coordinates become negatives, there will be a problem.
}

\value{GRanges object}

\author{Laure Le Calvez <laure.le-calvez@laposte.net>}

\examples{
regulatedExons = system.file("extdata", "regulatedExons.bed", package="RBPSpliceMap", mustWork=TRUE)
#Create GRanges object from a bedFile with exons of interest
data <- read.table(regulatedExons, header = TRUE)
colnames(data) = c('chr', 'start', 'end', 'ProbeID', 'pval', 'strand')
exonInterest = with(data, GRanges(chr, IRanges(start, end), strand))
#Treatment of the GRanges object with exons of Interest
exonInterestTreatment(exonInterest, spSite = "3SS", pE = 40, pI = 200)
}

\keyword{exon}
\keyword{splice site}
