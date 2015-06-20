## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {
    
    imat <- NULL # assigns null to imat
    # saves matrix and inverse in cache
    setM <- function(y) {
        mat <<- y 
        imat <<- NULL
    }
    # loads matrix from cache to current environment
    getM <- function() mat
    # saves inverse of a matrix in cache
    setInvMat <- function(InvMat) imat <<- InvMat
    # loads inverse of a marix from cache
    getInvMat <- function() imat
    # list of tools to access cache
    list(setM = setM, getM = getM,
         setInvMat = setInvMat,
         getInvMat = getInvMat)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # loads inverse from cache
        imat <- x$getInvMat()

    if(!is.null(imat)) {
        # if inverse is in cache writes
        message("getting cached data")
        # and returns inverse from cache
        return(imat)
    }
    # if inverse have not been computed yet
    # gets matrix from cache
    mat <- x$getM()
    # computes inverse
    imat <- solve(mat, ...)
    # saves inverse in cache
    x$setInvMat(imat)
    # returns the inverse
    imat

}
