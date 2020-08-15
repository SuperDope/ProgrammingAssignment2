## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix() creates a special matrix which is a list
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

## cacheSolve() computes the inverse of the special matrix that
## is returned by makeCacheMatrix(). If the inverse is calculated
## already, cached value is returned.

## For reference makeVector() code
## makeVector <- function(x = numeric()) {
##      m <- NULL
##   set <- function(y) {
##        x <<- y
##        m <<- NULL
##   }
##   get <- function() x
##   setmean <- function(mean) m <<- mean
##   getmean <- function() m
##   list(set = set, get = get,
##     setmean = setmean,
##     getmean = getmean)
## }

## Write a short comment describing this function
## First makeCacheMatrix derived from makeVector()
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## Return the inverse using solve()

## For reference, cacheMean()
## cachemean <- function(x, ...) {
##        m <- x$getmean()
##        if(!is.null(m)) {
##                message("getting cached data")
##                return(m)
##        }
##        data <- x$get()
##        m <- mean(data, ...)
##        x$setmean(m)
##        m
##}
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## Check if cached data is used
        ## if so, just return that.
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) ## Assumes only invertible matrix
        x$setinverse(m)
        m
}
