######################################################################################################################
# Interrater-reliability for dichotomeous ratings by > 2 raters for unequal number of raters per item
# Kappa (Fleiss & Cuzick, 1979)
# Code by Jana Jarecki, last changed 25. Dec. 2014
######################################################################################################################

kappaFC = function(N, ni, xi){
	# N =  total number of models
	# ni = number of judges rating the ith model
	# xi = number of positive judgments on the ith model
	p.i = 	xi/ni
	qi = 	1-p.i
	nbar = 	1/N * sum(ni) #mean number of judges
	pbar = 	1/(N*nbar) * sum(xi) #overall proportion positive judgments
	qbar =  1-pbar

	kappaFC = 1 - (sum(ni * p.i * qi) / (N * (nbar-1) * pbar * qbar))

	# Minimum
	minKappa = -1/(nbar-1)

	# Expectation and variance
	nbarH = N/sum(1/ni) #harmonic mean
	eKappaFC = -1/(N * (nbar-1)) #expected value
	varKappaFC = (2*(nbarH-1)) / (N*nbarH*(nbar-1)^2) + ((nbar-nbarH) * (1-4*pbar*qbar)) / (N*nbar*nbarH* (nbar-1)^2 * pbar*qbar) #variance

	return(list(kappa = kappaFC, harmonicMean = nbarH, expectkappa = eKappaFC, varianceKappa = varKappaFC, minKappa = minKappa, pbar = pbar))
}

cat("Properties of Fleiss-Cuzick Kappa:
		\n1. If there is no intersubject variation in the proportion of positive judgments (i.e., if pi, = pbar for all i), then there is less agreement (or more disagreement) among the judgments within than between the N subjects. In this case kappa may be seen to assume its minimum value of -1/(n-1)
		\n2. If the several proportions p, vary exactly as binomial proportions with parameters n, and a common probability pbar then there is as much agreement among the judgments within the subjects as there is between the subjects.
		     In this case, the value of kappa is equal to 0.
		\n3. If each proportion pi, assumes either the values 0 or 1, then there is perfect agreement among the judgments on each subject. In this case, kappa may be seen to assume the value 1.
		\n")


