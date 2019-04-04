## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix=function(mat=matrix()){
  i=NULL
  set=function(m){
    mat<<-m
    i=matrix(nrow=0,ncol=0)
  }
  get=function() mat
  setinv=function(inverse) i<<-inverse
  getinv=function() i
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve=function(mat,...){
  i=mat$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data=mat$get()
  i=solve(data,...)
  mat$setinv(i)
  i
}


