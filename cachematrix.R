## makeCacheMatrix creates a list that is used to cache a matrix and its inverse as well as nested
## functions for getting and setting the matrix as well as the inverse of the matrix
## cacheSolve is used to calculate the inverse of the matrix stored in the list created by makeCacheMatrix 
## and then store the inverse in the list created by makeCacheMatrix
## usage example:	m<-matrix(sample(1:50,49,replace=TRUE),7,7)
##					l<-makeCacheMatrix(m)
##					cacheSolve(l)

## makeCacheMatrix creates a list that is used to cache a matrix and its inverse as well as nested
## functions for getting and setting the matrix as well as the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ##return a list which caches x and the inverse of x and the nested functions
  ## to get/set the inverse and matrix
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inversematrix) im <<- inversematrix
  getInverseMatrix <- function() im
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## cacheSolve is used to calculate the inverse of the matrix stored in the list created by makeCacheMatrix 
## and then store the inverse in the list created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getInverseMatrix()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  
  data <- x$get()
  
  
  if(is.null(data)){
    message("input matrix is null")
    return(NULL)
  }
  
  if(nrow(data)!=ncol(data)){
    message("matrix is not square")
    return(NULL)
  }
  
  if(det(data)==0){
    message("matrix is not invertible") 
    ##return(NULL)
    return(NULL)
  }
  
  im <- solve(data)
  x$setInverseMatrix(im)
  im
}
