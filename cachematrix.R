## Set of functions to enable caching off matrices and their inverse.
## makeCacheMatrix: Generator function creating the list object that stores and writes the matrix and its inverse
## cacheSolve: Internal function to solve list matrices objects
## solve_inverse: Function to calculate the inverse of a matrix and create a list object whch stores the inverse aim was to override base::solve, but didn't have the time to implement a 'redirection' when input is for solving a linear system, and not just an matrix inversion




## Assign is used in place of <<- since this alows for specific nameing
## Assign(..., inerits = TRUE) is eqvivalent to ->> 
## If inherits is TRUE, enclosing environments of the supplied environment are searched until the variable x is encountered. The value is then assigned in the environment in which the variable is encountered (provided that the binding is not locked: see lockBinding: if it is, an error is signaled). If the symbol is not encountered then assignment takes place in the user's workspace(the global environment). help(assign)




makeCacheMatrix <- function(x = matrix()) {
  ## We are creating a new matrix so no cahce values should excist
  m <- NULL
  
  ## Is input a matrix, if not notify
  if(!is.matrix(x)) {
    message("Only matrix input accepted")
    return(m)
  }
  
  ## create matrix function, push Matrix to parent enviorment (makeCacheMatrix), and set m to NULL in parent enviorment
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## read matrix values
  getMatrix <- function() x
  ## Push inverse marix to parent enviorment (makeCacheMatrix) for further reference, ie m is not null 
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## if m is not calculated the value of null from the creation of the list object is in effect, and so no cached value will be reported, but a new will be calculated.
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  ## Get the matrix from the list object
  regMatrix <- x$getMatrix()
  ## solve - try should be implementet, so a singular matrix would return na)
  iMatrix <- base::solve(regMatrix)
  ## set the value in the list object
  x$setInverse(iMatrix)
  
}

solve_inverse <- function(x, name = NULL){
  ## if x is matrix we know it's not a cached type and we solve, and if name is provided a new object is silently created and the inverse is returned 
  if(is.matrix(x)) {
    ## try not implementet with error catching
    m <- try(base::solve(x))
    ## if name given and input is matric a new list matrix object is created in the parent enviorment
    if(!is.null(name)) { 
      a <- makeCacheMatrix(x)
      a$setInverse(m)
      assign(name, a, inherits = TRUE)
      a$setInverse(m)
     
    }
    return(m)
  }
  ## if list matrix object cacheSolve is caled
  if(is.list(x)) {
    m <- cacheSolve(x)
    return(m) 
  }
}
