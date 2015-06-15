## makeCacheMatrix function takes a square matrix as an input 
## and the inverse of the matrix is the output it caches the inverse value 
## and this cached value can be accessed via setMatrix and getMatrix


makeCacheMatrix <- function(p = matrix()) {
  m<-NULL
  set<-function(q){
    p<<-q
    m<<-NULL
  }
  get<- function()p
  setMatrix<-function(solve) m <<- solve
  getMatrix<-function() m
  list( set=set, get=get, setMatrix=setMatrix, getMatrix=getMatrix)
}


## cacheSolve function provides the inverse of the matrix if it is cached earlier 
## by calling getMatrix function of makeCacheMatrix function otherwise it computes 
## the value and adds the same to the cache by calling setMatrix function of makeCacheMatrix

##e.g.  a = matrix(c(2,3,4,5), nrow = 2,ncol = 2)
##abc<-makeCacheMatrix(a)
##cacheSolve(abc)
##Getting Not Cashed Value
##[,1] [,2]
##[1,] -2.5    2
##[2,]  1.5   -1
##
## running the same command again returns the cached value instead of computing
##cacheSolve(abc)
##Getting Cashed Value
##[,1] [,2]
##[1,] -2.5    2
##[2,]  1.5   -1
##

cacheSolve <- function(p,...) {
        ## Return a matrix that is the inverse of 'p'
  m<-p$getMatrix()
  if(!is.null(m)){
    message("Getting Cashed Value")
    return (m)
  }
  message("Getting Not Cashed Value")
  data<-p$get()
  m<-solve(data,...)
  p$setMatrix(m)
  m
}
