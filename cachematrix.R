## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## x<-makeCacheMatrix(A) will create a "global" variable (or "a buffering
## region") for the matrix A and its inverse.
## By using "<<-", the program updates this "global" variable, instead of
## modifying values locally.
## cacheSolve(x) will return the inverse of matrix A. (If it has not been


makeCacheMatrix <- function(x = matrix()) {
    mInverse<-NULL
	set<-function(y){
		x<<-y
		mInverse<<-NULL
	}
	get<-function() x
	setMatrixInverse<-function(InverseMatrix) mInverse<<- InverseMatrix
	getMatrixInverse<-function() mInverse
	list(set=set, get=get, setMatrixInverse=setMatrixInverse, getMatrixInverse=getMatrixInverse)
}


## Write a short comment describing this function

## computed, it will call solve() to calculate the inverse.)
## When running cacheSolve(x), the program will check whether mInverse
## is NULL. If not, it means it has been calculated and can be returned
## directly. Note: x is a list returned by x<-makeCacheMatrix(A).

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mInverse<-x$getMatrixInverse()
    if(!is.null(mInverse)){
        message("getting cached Matrix Inverse")
        return(mInverse)
    }
    matrix<-x$get()
    InverseMatrix<-solve(matrix, ...)
    x$setMatrixInverse(InverseMatrix)
    InverseMatrix
}
