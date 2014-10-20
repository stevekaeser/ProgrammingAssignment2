## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function takes a matrix x and provides the ability to cache
## it and it's inverse in a separate environment
## set is a function that stores the matrix (x) and since it's new, sets the inverse (i) to NULL
## get returns the matrix (x)
## setinverse sets the matrix inverse in the variable i
## getinverse returns the inverse (i)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## the cacheSolve function takes a value, previously returned from makeCacheMatrix and
## returns the inverse of the matrix represented in makeCacheMatrix.  If the inverse has
## previously been calculated, then the inverse (i) is taken from cache, otherwise it is
## calculated and stored in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

## example:
## create a test matrix
##> test <- matrix(c(2,2,3,2),nrow=2,ncol=2)
##
## use test to create a "cached" matrix with makeCacheMatrix
##> newmatrix <- makeCacheMatrix(test)
##
## use cacheSolve to get the inverse
## cacheSolve(newmatrix)
##     [,1] [,2]
##[1,]   -1  1.5
##[2,]    1 -1.0
##
## run it again to see it come from the cache
##> cacheSolve(newmatrix)
##getting cached data
##     [,1] [,2]
##[1,]   -1  1.5
##[2,]    1 -1.0