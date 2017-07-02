## This programming assignment required the student to write an R function able to 
## cache potentially time-consuming computations, for it may take too long to compute
## some functions if the element is a very long vector. The specific objective was
## to cache the inverse of a generic invertible matrix.

## The first function, creates a special "matrix" object that can cache its inverse
## through subfunctions that:
## 1. set the data of the matrix
## 2. get the data of the matrix
## 3. set the data of the inverse matrix
## 4. get the data of the inverse matrix

makeCacheMatrix<-function(x=matrix()){
  im<-NULL
  set<-function(y){
    y<<-x
    im<<-im
  }
  get<-function() x
  setInverse<-function(iMatrix) im<<-iMatrix 
  getInverse<-function() im
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## The second function calculates the inverse matrix using data from the special "matrix"
## above. However, it first checks if the inverse matrix has already been calculated. If so,
## it gets the inverse matrix from the cache and skips the computation. Otherwise, it
## calculates the inverse matrix and sets the result in the cache via setInverse() function.

cacheSolve<-function(x){
  im<-x$getInverse()
  if(!is.null(im)){
    message("Getting cached data")
    return(im)
  }
  data<-x$get()
  im<-solve(data)
  x$setInverse(im)
  im
}

