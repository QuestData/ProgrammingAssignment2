## To find the Inverse of a matrix
## caching included to improve perfomance

## Makes the matrix 

makeCacheMatrix <- function(x = matrix()) {
    ## initialize or reset inverse matrix 'm' as NULL
    m <- NULL
    ## assign value for the matrix
    set <- function(y= matrix()) {
        x <<- y
        m <<- NULL
    }
    ## Lists the matrix
    get <- function() x
    ## sets the inverse of matrix to 'm'
    setinverse <- function(solve) m <<- solve
    ## used to returns value of the inverse matrix 'm'
    getinverse <- function() m 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

## Checks cache to return the inverse of matrix'x'
## If no cache available calculates the inverse and sets its value in the cache

cacheSolve <- function(dmatrix, ...) {
    ## sets value of 'm' to the data got from getinverse
    m <- dmatrix$getinverse()
    ## checks if data is available in 'm'
    if(!is.null(m)){
        message("returns cached inverse")
        return(m)
    }
    ## accepts the data of the matrix 
    matrixdata<-dmatrix$get()
    ## calculates the inverse of the matrix
    m<-solve(matrixdata)
    ## stores the inverse value
    dmatrix$setinverse(m)
    message("returns calculated inverse")
    return(m)
        
}
