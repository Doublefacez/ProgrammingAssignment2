#week 3 peer assessment

makeCacheMatrix <- function(x = matrix()){
  mat <- NULL
  set<- function(y){
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mat <<-inverse
  getinverse <- function() mat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


a <- matrix(c(1:4),  nrow = 2, ncol = 2)
makeCacheMatrix(x=matrix(a))



cacheSolve <- function(x, ...){
  mat <- x$getinverse()
  
  # if the inverse of the matrix is calculated then skip the calculation 
 
  if(!is.null(mat)){
    message("getting cached data")
    return(mat)
  }
    
  #Otherwise calculate the inverse of the matrix
  
  data <- x$get()
  mat <- solve(data, ...)
  x$setinverse(mat)   #Putting the values of the inverse calculation to a matrix
  mat
}


#Testing section

B <- matrix(c(4,8,12,16),2,2)
makeCacheMatrix(B)
cacheSolve(makeCacheMatrix(B))
