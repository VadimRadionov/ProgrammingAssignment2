## The first function creates a special "special matrix" object -- a
## list of functions that store and retrive a matrix and its inverse.
## These values are stored not directly but as returned values of
## functions which are taken from environment where functions were
## defined.  The second function retrives the cashed value if it was
## set earlier, otherwise calculates, stores and returns it.

## makeCacheMatrix for a matrix argument x, creates a list of
## named functions to set and retrive the inverse of x, to retrieve x, and
## to (re)set x forgetting its cached inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function () x
    setinverse <- function (i) inverse <<- i
    getinverse <- function () inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve retrieves the inverse of its argument (which is "special
## matrix") from cache if it was calculated before, otherwise
## calculates it, stores the value in cache, and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Fetching cached inverse")
        return(inverse)
    }
    A <- x$get()
    a <- solve(A, ...)
    x$setinverse(a)
    a
}
