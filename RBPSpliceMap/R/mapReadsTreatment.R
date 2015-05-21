mapReadsTreatment <-
function(mapReadsGRanges, chromosome, strand, from, to){
	# Function: calculate the cover Vector of a part of interest
	# IN: GRanges of mapping of reads, strand, chromsome and coordinates of interest
	# OUT: Cover vector

	# Package imports nécessaires
	#library(GenomicAlignments)
	#library(GenomicRanges)
	
	### strand selection
	strandSpe = strandSelect(mapReadsGRanges, strand)
	
	# Check of the presence of reads:
	if (class(strandSpe) == "numeric")
		# Create a vector of 0 of interest ranges size
		coverVector = rep(0, to - from + 1) # the from and the to are include
	
	else {
		chromosomeSpe = chromosomeSelect(strandSpe, chromosome)
	
		# Check of the presence of reads:
		if (class(chromosomeSpe) == "numeric")
		# Create a vector of 0 of interest ranges size
		coverVector = rep(0, to - from + 1) # the from and the to are include
		
		else {
			### Ranges selection
			regionSpe = intervalSelect(chromosomeSpe, from, to)
			
			# Check of the presence of reads:
			if (class(regionSpe) == "numeric")
				# Create a vector of 0 of interest ranges size
				coverVector = rep(0, to - from + 1) # the from and the to are include
			
			else {
				### Cover
				coverChr = coverageChr(regionSpe, chromosome)
				
					# Check of the presence of reads:
				if (class(coverChr) == "numeric")
					# Create a vector of 0 of interest ranges size
					coverVector = rep(0, to - from + 1) # the from and the to are include
				
				else
					# Rle manipulation
					coverVector = coverageVector(coverChr, from, to)
			}
		}
	}
	# if the strand is "-" we have to reverse the vector
	# for it to be comparable to the strand "+" covers according to the splice site
	if (strand == "-")
		coverVector = rev(coverVector)
	
	return(coverVector) }
