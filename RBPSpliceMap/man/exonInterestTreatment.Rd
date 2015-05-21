\name{exonInterestTreatment}
\alias{exonInterestTreatment}

\title{exonInterestTreatment}

\description{
This function create a GRanges from another with modified coordinates given by the splice site chosen and the padding.
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
padding exon. Number of nucleotides wanted around the splice site on the exon
}
  \item{pI}{
padding intron. Number of nucleotides wanted around the splice site on the intron
}
}

\details{
If the input GRanges contain strand = "*", there are excluded, only strand = "+" or "-" are treated
The return GRanges contain coordinates of nucleotides framing ranges of interest around the splice site.
}

\value{GRanges object}

\author{Laure Le Calvez <laure.le-calvez@laposte.net>}

\examples{
regulatedExons = system.file("extdata", "regulatedExons.bed", package="RBPSpliceMap", mustWork=TRUE)
#Create GRanges from a bedFile with exons of interest
data <- read.table(regulatedExons, header = TRUE)
colnames(data) = c('chr', 'start', 'end', 'ProbeID', 'pval', 'strand')
exonInterest = with(data, GRanges(chr, IRanges(start, end), strand))
#Treatment of the GRanges with exons of Interest
exonInterestTreatment(exonInterest, spSite = "3SS", pE = 40, pI = 200)
}

\keyword{exon}
\keyword{splice site}
