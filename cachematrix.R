## This is a subset of two functions. The overall goal is to cache the
## inverse of a given matrix once it has been calculated to avoid 
## having to calculated it again for furture usage.


## The first function goes through multiple steps (1. setting the 
## value of the matrix, 2. getting the value of the matrix, 
## 3. setting the value of the inverse, 4. getting the value of the 
## inverse) to create a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solveMatrix) i <<- solveMatrix
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse, 
         getinverse = getinverse)
    
}


## This function takes the special "matrix" produced by makeCacheMatrix
## as an argument. It then first checks if the inverse has already been
## calculated and if so retrieves it from the cache. If it hasn't been
## calculated and cached yet, it does so now.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
    ## Returns a matrix that is the inverse of 'x'
}
