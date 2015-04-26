### ~~~ Assignment 2: Caching the Inverse of a Matrix ~~~ ###
#   ~~~      J.P. April 2015     ~~~     ###

# These two functions work together to store a matrix and its inverse in a cache.
# makeCacheMatrix() sets up the cache and stores the matrix.
# cacheSolve() ultimately returns the inverse of the matrix
#   by first checking to see if the inverse has already been calculated and 
#   stored in the cache (returning that value), then if no previous inverse
#   is stored it will calculate, store, and return the value of the matrix inverse

# Takes a square matrix and sets up a cache to store 
# the matrix and its inverse (which will be computed with cacheSolve later)
# Returns a list where each element of the list are the functions: 
# set(), get(), set.inv(), and get.inv()
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL     # a matrix inverse will eventually be placed in inv, but for now inv will be "empty"
    set <- function(y){ #y will be a matrix
        x <<- y #puts the matrix y into the cache (x)
        inv <<- NULL #deletes any previous matrix inverse stored in the cache
    }
    get <- function() x #retrieves the matrix x
    set.inv <- function(inverse) inv <<- inverse # puts a computed inverse into the cache (inv)
    get.inv <- function() inv #retrieves the cached inverse of matrix x
    list(set = set, get = get,  #returns the list of cache functions
         set.inv = set.inv,
         get.inv = get.inv)
}


# Takes the list of cache functions returned by makeCacheMatrix()
# Returns the inverse of the matrix ran through makeCacheMatrix()
cacheSolve <- function(x, ...) {
    inv <- x$get.inv()  # retrieves inverse value stored in the cache
    if(!is.null(inv)) { # if there was an inverse value (not NULL) stored in the cache,
        message("getting cached data") # returns it instead of recalculating it
        return(inv) # return() causes R to return the argument and stop running code for cacheSolve
    }
    data <- x$get() # retrieves the original matrix stored in the cache
    inv <- solve(data,...) # finds the inverse of that matrix
    x$set.inv(inv) # stores the inverse in the cache
    inv # returns the inverse of the matrix
}
