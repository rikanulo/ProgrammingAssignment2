# I'm not a native english speaker, so I apologize
# for my possible grammar mistakes in advance.

# These functions allow to cache the result of time-consuming
# matrix inversion, so that after the matrix's inverse is
# computed once during the first need to do so, the result is
# stored in memory and does not need to be computed again.

# This function returns a list of functions:
# "set" to set the matrix's value,
# "get" to get the matrix's value,
# "setinv" to cache the matrix's inverse,
# "getinv" to get the matrix's cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv = NULL
  
  set = function(y) {
    x <<- y
    inv <<- NULL
  }  
  get = function() x
  
  setinv = function(inverse) inv <<- inverse  
  getinv = function() inv
  
  list(set=set,
       get=get,
       setinv=setinv,
       getinv=getinv
  )
  
}

# This function calculates the inverse matrix, caches it
# and then returns (if getinv() returns null) or just
# returns the cached value if it has already been 
# calculated and cached

cacheSolve <- function(x, ...) {
  
  inv = x$getinv()
  
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  inv
  
}
