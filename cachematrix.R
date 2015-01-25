## We provide a constructor of our CacheMatrix object using makeCaheMatrix
## to access the cached inversed matrix we use cacheSolve

## Builds an object which contains the necessary methods to retrieve an 
## inversed cached matrix
## Takes as parameters the source matrix

makeCacheMatrix <- function(x = matrix()) {
  invertedMatrix <- NULL
  set <- function(y) {
    x <<- y
    invertedMatrix <<- NULL
  }
  get <- function() x
  setInvertedMatrix <- function(invertedMatrix) invertedMatrix <<- invertedMatrix
  getInvertedMatrix <- function() invertedMatrix
  list(set = set, get = get,
       setInvertedMatrix = setInvertedMatrix,
       getInvertedMatrix = getInvertedMatrix)
}


## returns the inverted matrix of x, using the cached value or calculating the
## inverted matrix and caching the value for future retrievals
## Takes as a parameter a CacheMatrix object, constructed with makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invertedMatrix <- x$getInvertedMatrix()
  if(!is.null(invertedMatrix)) {
    message("getting cached data")
    return(invertedMatrix)
  }
  matrix <- x$get()
  invertedMatrix <- solve(matrix, ...)
  x$setInvertedMatrix(invertedMatrix)
  invertedMatrix
}
