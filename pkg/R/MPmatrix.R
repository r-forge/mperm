MPmatrix <-
function(vec){
	if (!is.vector(vec)) stop("input must be a vector")
	m <- length(vec)
	if (m>5){
		cat("# of group > 5, plz take a smaller bite.\n")
		return(NULL)
	}
	if (!is.integer(vec)) vec <- as.integer(vec)
	for(i in 1:m){
		if(vec[i]==0){
			cat("Can't have group size of 0.");
			return(NULL);
		}
	}
	#get sum
	n <- 0;
	for (i in 1:m) n = n+vec[i];
	# calculate # of permutation
	nPerm <- factorial(n);
	for (i in 1:m) nPerm <- nPerm/factorial(vec[i]);
	cat("Total # of permutation is", nPerm, ".\n");
	
	mSize <- nPerm*m/1024/1024;
	cat("The estimated matrix size is", mSize, "Mega byte.\n");
	if(mSize > 512){
		cat("File size too large.\n");
		return(NULL);
	}
	
	.Call("MPmatrix", vec)
}

