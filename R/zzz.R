.First.lib <- function(libname, pkgname) {
	if(!is.loaded("MPmatrix")){
		dllpath <- file.path(libname, pkgname, "libs", "MPermutation.dll")
		dyn.load(dllpath)
	}
}