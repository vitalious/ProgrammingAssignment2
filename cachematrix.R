## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function (y, ...) {              # this function sets a new matrix which: 
                x <<- y                         # assigns the new matrix to 'x' 
                i <<- NULL                      # and flushes the cache
        }
        #define the functions that can be called within the list of the makeCacheMatrix function
        get <- function() {x}                   #retrieve the matrix
        setinv <- function(inv) {i <<- inv}     #sets inverse of the matrix to i, outside of the function
        getinv <- function() {i}                #retrieve the inverse of the matrix
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()     #fetch inverse
        if(!is.null(i)) {   #if it exists (not null) then returned the cached inverse
                message("getting cached data")
                return(i)
                break
        }
        #if there's no inverse, get the matrix, calculate the inverse, cache it, return it
        else {
                matrix <- x$get()
                i <- solve(matrix)
                x$setinv(i)
                return(i)        
        }
}
