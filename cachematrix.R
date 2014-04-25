## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {          ## This function will store a list of four items
        inv <- NULL
        set <- function(y) {                         ## After carefully setting everything to NULL                              
                x <<- y                              ## the first list item (set) is set
                inv <<- NULL
        }
        get <- function() x                          ## The other three items get set
        setinv <- function(inverse) inv <<- inverse  ## though I must admit that I don't exactly understand
        getinv <- function() inv                     ## what's going on
        list(set = set, get = get,                   
             setinv = setinv,                        
             getinv = getinv)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {                       ## If the inverse matrix is already cashed,
                message("getting cached data")    ## then return the message and the cashed matrix
                return(inv)
        }
        data <- x$get()                           ## Else
        inv <- solve(data, ...)                   ## the inverse matrix has to be calculated
        x$setinv(inv)
        inv                                       ## Return the solution
}
