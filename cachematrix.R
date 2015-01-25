## two functions below fro programming assignment 2:
## makeCacheMatrix acts as a 'instance' holding the matrix data via some lexical scoping
## cacheSolve inverts a matrix, but first checks if we have already inverteded it first and uses
## the 'cached' result

## setup a closure with a mutator and accessor 
## basically like a bean in Java land

makeCacheMatrix <- function( xInput = matrix()) {
  _xinverse <- NULL
  set <- function(y) {
    xInput <<- y
    # initialises _xinverse to null
    _xinverse <<- NULL 
  }
  
  # return the input matrix
  get <- function() xInput 
  # set the inversed matrix
  setInv <- function(inv) _xinverse <<- inv 
  # return the inversed matrix
  getInv <- function() _xinverse 
  # return as a list to mimic a object
  list(set = set, 
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## takes a matrix and computes the inverse
## if the inverse was already calculated, use that result 

cacheSolve <- function(x, ...) {
  # get the inversed matrix from object x
  m <- x$getInv() 
  # it will be null if uncalculated, remember the line 
  #"_xinverse <- NULL" in the previous function
  if(!is.null(m)) { 
    message("fetching cached data")
    return( m ) 
  }
  # if not 'cached', use x$get to get the matrix and invert it
  m <- solve(x$get()) 
  #store it
  x$setInv(m) 
  #return it
  m 
}


# Test code here
# generate a random square, non-singular, non-degenerate matrix
# has non zero determinant as well
myMatrix <- matrix(runif(9,1,1000),3,3)
# setup teh 'instance' variable
myMatrixCached <- makeCacheMatrix(myMatrix)

# run some tests and see after the first run, uses cache
testIt <- cacheSolve(myMatrixCached)
testIt
testIt <- cacheSolve(myMatrixCached)
testIt
testIt <- cacheSolve(myMatrixCached)
testIt <- cacheSolve(myMatrixCached)
testIt <- cacheSolve(myMatrixCached)
