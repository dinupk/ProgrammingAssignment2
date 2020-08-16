## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix<-NULL
  set<-function(y){
    x<<-y
    inverseMatrix<<-NULL
    
  }
  get<-function()x
  setInverse<-function(inverseValue){
    inverseMatrix<<-inverseValue
  }
  getInverse<-function()inverseMatrix
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMat<-x$getInverse()
  if(!is.null(inverseMat)){
    print("Getting cached inverted matrix")
    return(inverseMat)
  }
  
  data<-x$get()
  inverse<-solve(data)
  print("Not from cache")
  x$setInverse(inverse)
  inverse
}

#Initialize the new list type
mat<-makeCacheMatrix()
#initialize matrix
mat$set(matrix(1:4,2,2))

#first attempt! This will not be resolved from Cache.
inversem<-cacheSolve(mat)

#calling the cachesolve again to see whether the cached matrix will be retrived
inverse2<-cacheSolve(mat)

#Try out mulitplication of the inverted matrix with original matrix
#to verify that you get an identity matrix

mat$get()%*%inversem

#verify the cached matrix also is inverse by doing matrix multipplication 
#and checking for #identity matrix

mat$get()%*%inverse2
