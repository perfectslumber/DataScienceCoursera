## These two functions combined to chache the inverse of a matrix
## When a matrix (named mat here) is passed to makeCacheMatrix, it is cached
## cacheSolve(makeCacheMatrix(mat)) will retrieve the inverse of the cached matrix mat

## makeCacheMAtrix() creates a matrix object that can cache its inverse
## After passing mat to makeCacheMatrix by myMatrix <- makeCacheMatrix(mat),
## you can reset the matrix value by myMatrix$set(newmat), or retrieve the cached matrix by myMatrix$get()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve() computes the inverse of the matrix returned by makeCacheMatrix() function
## If the inverse has been calculated, it will retrieve the inverse from the cache
## It uses makeCacheMatrix()$get() to retrieve the matrix, calculate the inverse of it,
## then cache it by makeCacheMatrix()$setinv
## The cached matrix inverse can be retrieved by cacheSolve(makeCacheMatrix(mat))

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached matrix inverse")
        return(inv)
    }
    mydata <- x$get()
    inv <- solve(mydata, ...)
    x$setinv(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
