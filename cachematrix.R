##This function creates a special "matrix" object
##that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y) 
      {
        x <<- y
        m <<- NULL
      }
  get <- function() x
  setsolve <- function(solve) m <<- solve #solve() function
  getsolve <- function() m
  #List of the functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  }

# This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(cacheFUNs, ...) 
{
  m <- cacheFUNs$getsolve()
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  else
  {
    message("calculating new data")
  }
  data <- cacheFUNs$get()
  m <- solve(data, ...)
  cacheFUNs$setsolve(m)
  m
}

##TESTS
# amatrix = makeCacheMatrix(matrix(1:4, 2))
# cacheSolve(amatrix)   # Computes, caches, and returns matrix inverse
# cacheSolve(amatrix)   # Returns cached matrix inverse
# 
# amatrix$set(matrix(12:14, 2)) # Modify existing matrix
# cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
# cacheSolve(amatrix)   # Returns cached matrix inverse
