\name{bamToGRanges}
\alias{bamToGRanges}

\title{bamToGRanges}

\description{
This function transform a bam file into a GRanges object
}

\usage{
	bamToGRanges(bamFile)
}

\arguments{
  \item{bamFile}{\code{bamFile} is path of a bam file}
}

\details{
The return GRanges Object contain "seqnames", IRanges(start,end) and "strand". It is possible to have access to the different values vector with :
- seqnames(<GRangesName>)
- start(ranges(<GRangesName>)
- end(ranges(<GRangesName>)
- strand(<GRangesName>)
}

\value{GRanges object}

\author{Laure Le Calvez <laure.le-calvez@laposte.net>}

\seealso{
\code{\link{readGAlignments}} from package GenomicAlignments
\code{\link{as}}
}

\examples{
iCLIPData = system.file("extdata", "iCLIPData.bam", package="RBPSpliceMap", mustWork=TRUE)
#Create a GRanges from a bam file
bamToGRanges(iCLIPData)
}

\keyword{bam}
