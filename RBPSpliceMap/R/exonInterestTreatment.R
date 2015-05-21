exonInterestTreatment <-
function (exonInterest, spSite, pE, pI) {
	# Function: modify the coordinates of the GRanges according to the padding and the splice site
	# OUT: GRanges 
	
	# pI will always be used as pI-1 because 3'SS and 5'SS on the intro, excluded of the exon
	pI = pI - 1
	
	# Subset of line with strand == "*"
	exonInterest = subset(exonInterest, strand != "*")
	
	# Creation of an other GRanges object identic to the other to remember the initial coordinates
	exonInterestMemory = exonInterest
	
	# Modification of coordinates to have coordonates wanted around the splice Site
	# Depending on the spliceSite, strand and padding chosen
	if (spSite == "3SS") {
		end(ranges(exonInterest[strand(exonInterest) == "+"])) = (start(ranges(exonInterestMemory[strand(exonInterestMemory) == "+"])) + pE)
		start(ranges(exonInterest[strand(exonInterest) == "+"])) = (start(ranges(exonInterestMemory[strand(exonInterestMemory) == "+"])) - pI)
		start(ranges(exonInterest[strand(exonInterest) == "-"])) = (end(ranges(exonInterestMemory[strand(exonInterestMemory) == "-"])) - pE)
		end(ranges(exonInterest[strand(exonInterest) == "-"])) = (end(ranges(exonInterestMemory[strand(exonInterestMemory) == "-"])) + pI)
	}
	
	else if (spSite == "5SS") {
		start(ranges(exonInterest[strand(exonInterest) == "+"])) = (end(ranges(exonInterestMemory[strand(exonInterestMemory) == "+"])) - pE)
		end(ranges(exonInterest[strand(exonInterest) == "+"])) = (end(ranges(exonInterestMemory[strand(exonInterestMemory) == "+"])) + pI)
		end(ranges(exonInterest[strand(exonInterest) == "-"])) = (start(ranges(exonInterestMemory[strand(exonInterestMemory) == "-"])) + pE)
		start(ranges(exonInterest[strand(exonInterest) == "-"])) = (start(ranges(exonInterestMemory[strand(exonInterestMemory) == "-"])) - pI)
	}

	return(exonInterest)
}
