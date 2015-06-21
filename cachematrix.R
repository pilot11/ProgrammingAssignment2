# Description
##
## There are two function that caching the inverse of a matrix.
## The inverse of a matrix be cache after the first time compute.
## Later use the inverse matrix just get the caching one instead compute it again.
##
## Function makeCacheMatrix provide "setinv" and "getinv" functions to caching 
## the inverse matrix and return it.
##
## Function cacheSolve return the inverse matrix. "cacheSolve" compute the inverse matrix
## and cache it at the first time called,and just get it from cache at later called.

# Examples
##
## mat <- matrix(1:4,2)
## cachemat <-makeCacheMatrix(mat)
##
## 2 * cacheSolve(cachemat)
## 3 + cacheSolve(cachemat)
## ...



## The inverse matrix and return it.
## The matrix data caching in "x" variable and the inverse caching in "inv".
## "set" and "setinv" function caching the data.
## "get" and "getinv" function get data from cache.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(mat) {
        x <<- mat
        inv <<- NULL
    }

    get <- function() {
        x
    }
    
    setinv <- function(invmat) {
        inv <<- invmat
    }
    
    getinv <- function() {
        inv
    }

    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Get the inverse matrix. 
## If there is not inverse data in cache,compute it and cahce it,then return it.
## If the inverse matrix data already exist in cache,get and return it. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    if(!is.null(inv)){
        message("getting cached inverse")
    }else{
        message("compute inverse")
        mat <- x$get()
        inv <- solve(mat,...)
        x$setinv(inv)
    }
    inv
}
