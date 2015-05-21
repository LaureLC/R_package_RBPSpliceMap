coverageChr <-
function(mapReadsGRanges, chromosome) {
	# Function: calculates the cover and give the one of the chromosome of interest
	# OUT: cover (Rle // package: S4Vectors)
	
	# If there is no more line in the GRanges (no read matches to interest ranges)
	# In this case, we return 0 to create a vector of zeros at the last step
	if (length(mapReadsGRanges) == 0)
		coverChr = 0
	
	else {
		# We delete known seqlengths to be able to have a cover on the part of interest
		chrLength = rep(NA, length(seqlengths(mapReadsGRanges)))
		seqlengths(mapReadsGRanges) = chrLength
		
		coverAll = coverage(mapReadsGRanges)	# SimpleRleList object//package: IRanges
	
		### Chromosome selection
		# If the chromosome has already been selected, there are only zeros in others but we still have to select
		coverChr = coverAll[[chromosome]]	# Rle object // package: S4Vectors
		
		# If there is no more line in the GRanges (no read matches to interest ranges)
		# In this case, we return 0 to create a vector of zeros at the last step
		if (length(coverChr) == 0)
			coverChr = 0
	}
	return(coverChr) }
