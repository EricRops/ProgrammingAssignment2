## See comments inside the functions which explain what they do

makeCacheMatrix <- function(x = matrix()) {
    ## Input: x, a square inverible matrix 
    ## Output: a special "matrix", which is really a list containing functions to:
    ## 1. set the values of the matrix
    ## 2. get the values of the matrix
    ## 3. set the inverse of the matrix
    ## 4. get the inverse of the matrix
    
    ## The <<- operator assigns a value to See an object in an environment that is 
    ## different from the current environment.
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


cacheSolve <- function(x, ...) {
    ## Input: x, output from makeCacheMatrix.R
    ## Output: the inverse of the special "matrix." If the inverse has already 
    ## been calculated (and the matrix has not changed), then the inverse is retrieved 
    ## from the cache.
    
    inv <- x$getinv()
    
    ## If inverse already exists, get it from the cache and skip computation
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## If inverse does not exist, calculate it using solve. Then store the inverse
    ## in the cache via the setinv function
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    return(inv)
}
