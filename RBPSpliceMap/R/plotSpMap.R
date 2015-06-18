plotSpMap <-
function(spMapList, ylim=c(0,max(spMapList[[1]])), type = "s", color = "black"){
	# Recovery of results, from the list given by the user, separately
	readsNumber = spMapList[[1]]
	spSite = spMapList[[2]]
	pE = spMapList[[3]]
	pI = spMapList[[4]]-1
	# pI is use as -1 because the splice Site is a part of the intron and we calculate from the splice Site

	# Different representations depending on the splice Site
	if (spSite == "3SS") {
		bp = -pI:pE		# give the x coordonates (number of base pair
		par(mfrow = c(2,1),mai = c(0.8, 1, 0.1, 1), oma = c(5,1,1,1))	# Give the possibility to put 2 diagram, one above the other
		boxplot(c(-pI,1,pE,pE,pE), horizontal=TRUE, axes = FALSE, lty = "solid", lwd = 2, range = pI)	# to represent the exon
		#Little text to indicate the splice Site and annotate the exon
		mtext("3'SS", side = 3, line = -2, at = 0)
		# cover graph
		plot(bp, readsNumber, type = type,ylim = ylim, col = color) }
	
	else {
		bp = -pE:pI
		par(mfrow = c(2,1),mai = c(0.8, 1, 0.1, 1), oma = c(5,1,1,1))
		boxplot(c(-pE,-pE,-pE,-1,pI), horizontal=TRUE, axes = FALSE, lty = "solid", lwd = 2, range = pI)
		mtext("5'SS", side = 3, line = -2, at = 0)
		plot(bp, readsNumber, type = "s", ylim = ylim, col = color) }
}
