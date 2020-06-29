## To cache the inverse of a matrix

## This function creates a special matrix to cache the inverse

makeCacheMatrix<-function(m=matrix())
{
  mx<-NULL
  set<-function(x)
  {
    m<<-x
    mx<<-NULL
  }
  get<- function() m
  setinv<-function(solve) mx<<- solve
  getinv<-function() mx
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function computes the inverse of the special matrix
## returned by the above function

cacheSolve<-function(m)
{
  mx<-m$getinv()
  if(!is.null(mx))
  {
    message("getting cached data")
    return(mx)
  }
  data<-m$get()
  mx<-solve(data)
  m$setinv(mx)
  mx
}
