## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function create a list of functions to set and get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set=set, get=get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## function to check if previous calculated cache exists, if yes, return the cache, 
## otherwise, calculate the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached inverse matrix")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}


## to test above two functions.
x <- matrix(rnorm(16), 4, 4)
y <- makeCacheMatrix(x)
z <- cacheSolve(y)
x %*% z   ## this result should return matrix of 4 x 4 with 1 from top left to bottom right

