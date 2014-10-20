## Assignment 2: 
# Below two R functions have been defined: makeCacheMatrix & cacheSolve in order
# to cache the inverse of a matrix when the matrix has not changed so that, when 
# we need it again, it can be looked up in the cache rather than recomputed.

# makeCacheMatrix: is a function which creates a special "matrix" object that
# can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {             # input x will be a matrix
        i <- NULL                                       # i will be the inverse and it's reset to NULL every 
                                                        # time makeCacheMatrix is called
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x                             # this function returns the value of the original vector
        setinverse <- function(inverse) i <<- inverse   # this is called by cacheSolve() during the first cacheSolve()
        getinverse <- function() i                      # this will return the cached value to cacheSolve()
        list(set = set, get = get,                      # this list is returned with the newly created object
             setinverse = setinverse,
             getinverse = getinverse)
}

# cacheSolve: is a function which computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated
# and the matrix has not changed then cacheSolve should retrieve the inverse from
# cache.

cacheSolve <- function(x, ...) {                        # the input is an object created by makeCacheMatrix
        i <- x$getinverse()                             # accesses the object 'x' and gets the value of the inverse
        if(!is.null(i)) {                               # if inverse was already cached (not NULL)
                message("getting cached data")          # send this message to the console
                return(i)                               # and return the inverse   
        }
        data <- x$get()                                 # if inverse wasn't cached read the matrix
        i <- solve(data, ...)                           # calculate its inverse 
        x$setinverse(i)                                 # store the the calculated inverse in x  
        i                                               # and return the inverse 
}
