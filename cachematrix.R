# The following functions can be used to calculate the inverse matrix
# and save the results to a cache. In order to reduce processing time
# if the input matrix is identical to the matrix on cache, the function
# returns the inverse matrix already saved on cache.

# The first function creates a list of functions to set 
# and get the matrix and its inverse to/from a cache.

makeCacheMatrix <- function(x=matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      seti_matrix <- function(i_matrix) m <<- i_matrix
      geti_matrix <- function() m
      list(set = set, get = get,
           seti_matrix = seti_matrix,
           geti_matrix = geti_matrix)
}



# The second function compares the input matrix to the cache.
# if it's identical it returns the inverse matrix saved on cache
# if it's not, it calculates the new inverse matrix and 
# refresh the cache

cachesolve <- function(matrix, ...) {
      
      m <- x$geti_matrix()
      matrix_Cache <- x$get()
      
      if(!identical(matrix,matrix_Cache)){
            x$set(matrix)
            m <- NULL
      }
      
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$seti_matrix(m)
      m
}
