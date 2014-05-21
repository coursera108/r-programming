#makeCacheMatrix caches the inverse of a matrix.
# Example Usage
# a <- makeCacheMatrix() #Creates a Cache
# a$set(m1) #m1 is a matrix
# inverse.m1 <- cacheSolve(a) #Returns the inverse. Calculates inverse on the first call. Subsequent calls return cached value

# Function that caches the inverse 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  getSolve <- function() m
  setSolve <- function(result) m <<- result
  list(set = set, get = get, getSolve=getSolve,
       setSolve=setSolve)
}


# Function that computes the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setSolve(m)
  m
}
