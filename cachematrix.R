## Computing and Caching the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y                    #set the value
    n <<- NULL               #clear the old inverse from cache
  }
  ## Define function to get the value of the matrix
  get <- function()
    x
  ## Define function to set the inverse to be used by getinverse() when
  ## there is no cached inverse
  setInverse <- function(inverse)
    n <<- inverse
  ##Define function to get the inverse
  getInverse <- function()
    n
  ## Return a list with the above four functions
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix .If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  n <- x$getInverse() # This fetches the cached value for the inverse
  if (!is.null(n)) {
    ## If the cache was not empty, we can just return it
    message("getting cached data")
    return(n)
  }
  ## As the cache was empty,we need to re-calculate it, cache it, and then return it.
  indata <- x$get()  # Get value of matrix
  n <- solve(indata) # Calculate inverse
  x$setInverse(n)  # Cache the result
  n               # Return the inverse
}
