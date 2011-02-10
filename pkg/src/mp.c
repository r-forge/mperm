/*
Author: Kun Liang
liangkun@iastate.edu
*/

#include <stdio.h>
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

double factorial(int n){
	int i;
	if(n<1) return 0;
	double retVal = 1;
	for(i=1; i<=n; i++) retVal = retVal*i;
	return retVal;
}

/* The actually multiset permutation is done here recursively.
inputs:
Rval, the matrix store the result
iLeadingSize, length of the iLeading
iLeading, the assigned groups
iGroupSize, number of groups left
nVec, the array of group sizes
iVec, the array of group identifier
*/
void mpm(int *iRval, int iLeadingSize, int *iLeading, int iGroupSize, int *nVec, int * iVec, int nPerm, int *iRow, int *iCol){
	int i, j;
	int *newNVec;
	int *newLeading;
	if(iGroupSize==1){
		for(i=0; i<iLeadingSize; i++){
			iRval[(*iRow)+(*iCol)*nPerm] = iLeading[i];
			(*iCol)++;
		}
		for(i=0; i<nVec[0]; i++){
			iRval[(*iRow)+(*iCol)*nPerm] = iVec[0];
			(*iCol)++;
		}
		//next Permutation, reset the row and col counter
		(*iRow)++;
		*iCol = 0;
		return;
	}
	for(i=0; i<iGroupSize; i++){
		newLeading = (int *) calloc(iLeadingSize+1, sizeof(int));
		for(j=0; j<iLeadingSize; j++) newLeading[j] = iLeading[j];
		newLeading[iLeadingSize] = iVec[i];
		if(nVec[i]==1){
			//one group will be empty, do it forward
			//copy the non-empty
			newNVec = (int *) calloc(iGroupSize-1, sizeof(int));
			for(j=0; j<i; j++) newNVec[j] = nVec[j];
			for(j=i+1; j<iGroupSize; j++) newNVec[j-1] = nVec[j];
			int *newIVec;
			newIVec = (int *) calloc(iGroupSize-1, sizeof(int));
			for(j=0; j<i; j++) newIVec[j] = iVec[j];
			for(j=i+1; j<iGroupSize; j++) newIVec[j-1] = iVec[j];
			mpm(iRval, iLeadingSize+1, newLeading, iGroupSize-1, newNVec, newIVec, nPerm, iRow, iCol);
			//free
			free(newIVec);
		}else{
			newNVec = (int *) calloc(iGroupSize, sizeof(int));
			for(j=0; j<i; j++) newNVec[j] = nVec[j];
			newNVec[i] = nVec[i]-1;
			for(j=i+1; j<iGroupSize; j++) newNVec[j] = nVec[j];
			mpm(iRval, iLeadingSize+1, newLeading, iGroupSize, newNVec, iVec, nPerm, iRow, iCol);
		}
		free(newNVec);
		free(newLeading);
	}
}

/* main entrance, read in group size vector, create matrix, call subroutine.*/
SEXP MPmatrix(SEXP Rvec){
	int i, m, n;
	double nPerm, mSize;
	int *vec;
	SEXP Rval;
	Rvec = coerceVector(Rvec, INTSXP);
	vec = INTEGER(Rvec);
	m = length(Rvec);
	//check size
	if(m > 10){
		printf("# of group > 10, plz take a smaller bite.\n");
		return R_NilValue;
	}

	for(i = 0; i < m; i++){
		if(vec[i]==0){
			printf("Can't have group size of 0.");
			return R_NilValue;
		}
	}
	
	printf("The group sizes are: ");
	for (i = 0; i < m-1; i++) printf("%d, ", vec[i]);
	printf("%d \n", vec[m-1]);

	//get sum
	n = 0;
	for (i = 0; i < m; i++) n += vec[i];
	//calculate # of permutation
	nPerm = factorial(n);
	for (i = 0; i < m; i++) nPerm /= factorial(vec[i]);
	printf("Total # of permutation is %g.\n", nPerm);
	
	mSize = nPerm*m/1024/1024;
	printf("The estimated matrix size is %g Mega byte.\n", mSize);
	if(mSize > 512){
		printf("File size too large.\n");
		return R_NilValue;
	}
	
	//create symbol array
	int * iSymbol;
	iSymbol = (int *) calloc(m, sizeof(int));
	for (i = 0; i < m; i++) iSymbol[i] = i;

	/*Create the return matrix*/
	PROTECT(Rval = allocMatrix(INTSXP, nPerm, n));
	int *iRval = INTEGER(Rval);

	int iRow=0, iCol=0;
	int *temp;
	mpm(iRval, 0, temp, m, vec, iSymbol, nPerm, &iRow, &iCol);

	UNPROTECT(1);

	return Rval;
	
}

/**************************************************************/

/* The actually multiset permutation is done here recursively.*/
void mpf(FILE *fp, int cSize, char *leading, int vSize, int *nVec, char * cVec){
	int i, j;
	int *newNVec;
	char *newLeading;
	if(vSize==1){
		for(i=0; i<cSize; i++) putc(leading[i], fp);
		for(i=0; i<nVec[0]; i++) putc(cVec[0], fp);
		fputs("\n", fp);
		return;
	}
	for(i=0; i<vSize; i++){
		newLeading = (char *) calloc(cSize+1, sizeof(char));
		for(j=0; j<cSize; j++) newLeading[j] = leading[j];
		newLeading[cSize] = cVec[i];
		if(nVec[i]==1){
			//one group will be empty, do it forward
			//copy the non-empty
			newNVec = (int *) calloc(vSize-1, sizeof(int));
			for(j=0; j<i; j++) newNVec[j] = nVec[j];
			for(j=i+1; j<vSize; j++) newNVec[j-1] = nVec[j];
			char *newCVec;
			newCVec = (char *) calloc(vSize-1, sizeof(char));
			for(j=0; j<i; j++) newCVec[j] = cVec[j];
			for(j=i+1; j<vSize; j++) newCVec[j-1] = cVec[j];
			mpf(fp, cSize+1, newLeading, vSize-1, newNVec, newCVec);
			//free
			free(newCVec);
		}else{
			newNVec = (int *) calloc(vSize, sizeof(int));
			for(j=0; j<i; j++) newNVec[j] = nVec[j];
			newNVec[i] = nVec[i]-1;
			for(j=i+1; j<vSize; j++) newNVec[j] = nVec[j];
			mpf(fp, cSize+1, newLeading, vSize, newNVec, cVec);
		}
		free(newNVec);
		free(newLeading);
	}
}

/* main entrance, read in group size vector, create file handle, call subroutine.*/
SEXP MPfile(SEXP Rvec){
	int i, m, n;
	double nPerm, mSize;
	int *vec;
	char *vsymbol;
	Rvec = coerceVector(Rvec, INTSXP);
	vec = INTEGER(Rvec);
	m = length(Rvec);
	//check size
	if(m > 10){
		printf("# of group > 10, plz take a smaller bite.\n");
		return R_NilValue;
	}

	for(i = 0; i < m; i++){
		if(vec[i]==0){
			printf("Can't have group size of 0.");
			return R_NilValue;
		}
	}
	
	printf("The group sizes are: ");
	for (i = 0; i < m-1; i++) printf("%d, ", vec[i]);
	printf("%d \n", vec[m-1]);

	//get sum
	n = 0;
	for (i = 0; i < m; i++) n += vec[i];
	//calculate # of permutation
	nPerm = factorial(n);
	for (i = 0; i < m; i++) nPerm /= factorial(vec[i]);
	printf("Total # of permutation is %g.\n", nPerm);
	
	mSize = nPerm*m/1024/1024;
	printf("The estimated output file size is %g Mega byte.\n", mSize);
	if(mSize > 500){
		printf("File size too large.\n");
		return R_NilValue;
	}
	
	//create symbol array
	char * vSymbol;
	vSymbol = (char *) calloc(m, sizeof(char));
	for (i = 0; i < m; i++) vSymbol[i] = 48+i;
	//open file
	FILE *fp;
	fp = fopen("output.txt", "w");
	char *temp;
	if (fp == NULL){
		printf("Error opening file.\n");
	}
	else{
		mpf(fp, 0, temp, m, vec, vSymbol);
	}

	//close file
	fclose(fp);
	printf("Permutation of the group has been created in file output.txt.\n");
	return R_NilValue;
	
}