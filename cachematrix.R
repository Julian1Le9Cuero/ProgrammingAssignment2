# makeCacheMatrix will provide methods to cacheSolve to
# set the cachedMatrix, cachedInverse if we have a matrix
# different from the previously provided

makeCacheMatrix <- function(newM=matrix(c(2,0,0,0,5,0,0,0,4),3,3)){

    getCachedMatrix <- function() {
        # Avoid error if cachedMatrix hasn't been set
        if(exists("cachedMatrix")) {
            return(cachedMatrix)
        } else{
            return(NULL)
        }
    }

    getCachedInverse <- function() {
        if(exists("cachedInverse")) {
            return(cachedInverse)
        } else{
            return(NULL)
        }
    }

    setCachedMatrix <- function(M) cachedMatrix <<- M
    setCachedInverse <- function(inv) cachedInverse <<- inv


    cacheSolve(list(newM=newM,
                    getCachedMatrix=getCachedMatrix,
                    getCachedInverse=getCachedInverse,
                    setCachedMatrix=setCachedMatrix,
                    setCachedInverse=setCachedInverse
    ))
}

# cacheSolve calculates the Inverse if the matrix is new,
# otherwise, it will return the cached inverse so the same inverse
# is not calculated every time

# NOTE: In this case, we assume that the matrix supplied
# is always invertible
cacheSolve <- function(x, ...){
    cachedMatrix <- x$getCachedMatrix()

    #Check if we have a cached Matrix
    if(!is.null(cachedMatrix) & identical(cachedMatrix, x$newM)){
        message("Caching inverse..")
        return(x$getCachedInverse())
    }

    message("Calculating inverse..")
    newMatrix <- x$newM
    x$setCachedMatrix(newMatrix)
    newInverse <- solve(newMatrix)
    x$setCachedInverse(newInverse)
    newInverse
}
