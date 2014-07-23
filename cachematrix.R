## The functions below calculate the inverse of matrix.
## The result is stored in cache memory and retrieved when needed
##  to avoid potentially timeconsuming recompuration?


## The function "makeCacheMatrix" creates a special "matrix" object
## that can cache its inverse
## "X" is the cached matrix
## "I" is the cached inverse of the matrix

makeCacheMatrix <- function (X = numeric()) {
    
    I <- NULL
    
    set <- function (Y) {
        X <<- Y      # X is assigned in the parent environment
        I <<- NULL   # I is assigned in the parent environment
    }
    
    get <- function () X
    setInverse <- function (XI) I <<- XI # I is assigned in the parent env.
    getInverse <- function () I
    
    # returns a list of functions to get and set "X" or "I"
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function "cacheSolve" computes the inverse of the special "matrix"
## returned by "makeCacheMatrix" above. If the inverse has already
## been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache.
## Note: if the "matrix" is not inversible then a message is returned

cacheSolve <- function (X,...) {
    
    XI<- X$getInverse()
    
    # returns XI if already calculated
    if (!is.null(XI)) {
        message("getting cache data")
        return (XI)
    }
    
    # else retrieve "matrix" and make calculation
    data <- X$get()
    
    # if determinant equal zero it means the matrix is not inversible
    if (det(data)==0) {
        message("not inversible")
        return (XI)
    }
    XI <- solve(data)
    X$setInverse(XI)
    XI
    
} 
