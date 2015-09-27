## "cacheSolve" and "makeCacheMatrix" are functions that allow reuse 
## of the calculation of the inverse of a matrix. In operations 
## involving the inverse of a matrix, while the source matrix 
## is not altered then the inverse is computed only once and 
## the result is reused as necessary.

## The function "makeCacheMatrix" creates an object that stores
## the original matrix and the inverse of the matrix. 
## The object also provides functions to read and write 
## the original and inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    cm <- NULL
    
    set <- function(nm) {
        x <<- nm
        cm <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(im) {
        cm <<- im
    } 
    
    getinverse <- function() {
        cm
    }
    
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## The function "cacheSolve" gets the inverse of the matrix 
## from the object created by the makeCacheMatrix function. 
## If the inverse is already computed then it returns the 
## inverse matrix stored in the cache. If the inverse is 
## not computed then it calls the solve function to compute 
## it and then store it in the cache so that it can be reused again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getinverse()
    
    if(!is.null(im)) {
        message("getting inverse matrix")
        return(im)
    }
    
    m <- x$get()
    im <- solve(m, ...)
    x$setinverse(im)
    im
}
