intervalSelect <-
function(objectGRanges, from, to) {
	# Function: Select, in the GRanges, ranges of interest
	# OUT : GRanges object (exactly the ranges)	// package: GenomicRanges
	
	objectFrom = subset(objectGRanges, end >= from)
	grRegion = subset(objectFrom, start <= to)
	
	# If there is no more line in the GRanges (no read matches to interest ranges)
	# In this case, we return 0 to create a vector of zeros at the last step
	if (length(grRegion) == 0)
		grRegion = 0
	
	else {
		## Modification of IRanges of the beginning and the end
		# To have the same sequence's length and then same vector's length for rle
		# 2 or more start/ end can exceed the limit
		# we do it for each which exceed the limit
		start(ranges(grRegion[start(ranges(grRegion)) < from])) = from
		end(ranges(grRegion[end(ranges(grRegion)) > to])) = to
	}
	return(grRegion) }
