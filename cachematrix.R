## makeCachMatrix and cacheSolve are two functions that enable the caching of the inverse of a matrix utilizing the lexical scoping rule of R language

## makeCacheMatrix takes a single input argument of an invertible matrix and output a list of 4 functions (set, get, setiverse, getinverse) to achieve setting/getting the original matrix, as well as setting/getting the inverse matrix
makeCacheMatrix <- function(mat = matrix()) {
    inverse_mat <- NULL
    set <- function(mat_new) {
        mat <<- mat_new
        inverse_mat <<- NULL
    }
    get <- function() mat
    setinverse <- function(inverse_mat_new) inverse_mat <<- inverse_mat_new
    getinverse <- function() inverse_mat
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes an input argument consisting of the object created using makeCacheMatrix, and search for a cached inverse matrix, and if it exists then it will return the inverse matrix, otherwise it will calculate the inverse matrix via the solve function and cache the inverse matrix via a function call to "setinverse" in the makeCacheMatrix object
cacheSolve <- function(x) {
    inverse_mat <- x$getinverse()
    if(!is.null(inverse_mat)) {
        message("getting cached inverse matrix")
        return(inverse_mat)
    }
    data <- x$get()
    inverse_mat <- solve(data)
    x$setinverse(inverse_mat)
    inverse_mat
}
