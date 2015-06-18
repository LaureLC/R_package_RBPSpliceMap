\name{bamToGRanges}
\alias{bamToGRanges}

\title{bamToGRanges}

\description{
This function uses the readGAlignments function to read a bamfile and creates a GRanges object.
}

\usage{
bamToGRanges(bamFile)
}

\arguments{
  \item{bamFile}{\code{bamFile} is the path of a bam file}
}

\details{
The returned GRanges object contains "seqnames", IRanges(start, end) and "strand". It is possible to have access to the different values vector with :\cr
- seqnames(<GRangesName>)\cr
- start(ranges(<GRangesName>))\cr
- end(ranges(<GRangesName>))\cr
- strand(<GRangesName>)
}

\value{GRanges object}

\author{Laure Le Calvez <laure.le-calvez@laposte.net>}

\seealso{
\code{\link{readGAlignments}} from package GenomicAlignments\cr
\code{\link{as}}
}

\examples{
iCLIPData = system.file("extdata", "iCLIPData.bam", package="RBPSpliceMap", mustWork=TRUE)
#Create a GRanges object from a bam file.
bamToGRanges(iCLIPData)
}

\keyword{bam}
