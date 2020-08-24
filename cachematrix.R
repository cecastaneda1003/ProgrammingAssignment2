###### R Programming Assignment 2: Lexical Scoping ######
#Castañeda Cecilia#
##Su tarea es escribir un par de funciones que almacenan en caché la 
##inversa de una matriz.

##La función 'makeCacheMatrix' crea un objeto "matriz" especial que puede 
##almacenar su inverso en caché.

makeCacheMatrix <- function(x=matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##La función 'cacheSolve' calcula la inversa de la "matriz" especial 
##devuelta por la función 'makeCacheMatrix' obtenida ya con anterioridad.
##Si ya se ha calculado la inversa (y la matriz no ha cambiado), entonces 
##la función 'cacheSolve' debería recuperar la inversa que se encuentra 
##en elcaché.

cacheSolve <- function(x, ...) {
  ## Devuelve una matriz que es la inversa de 'x'##
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

###########################
##TESTEANDO LAS FUNCIONES##
###########################

#############################
##1. Con numeros aleatorios##
#############################

print('Ingrese el número de columnas de su matriz cuadrada en la consola y aplaste ENTER')
n<-scan(file = "",what = numeric(),n = 1)
matriZ1<-sample(1:200,n*n,replace=F)
matriZ1<-matrix(matriZ1,n,n)
print('Su matriz cuadrada es la siguiente:')
print(matriZ1)
class(matriZ1)
dm<-det(matriZ1)
if (dm==0){
  print("No existe la inversa de su matriz por que el determinante es 0")
}else{
  print(paste0('El determinante es igual a:', dm))
  my_matrix<- makeCacheMatrix(matriZ1) 
  #Creo un matriz evaluada en la función makeCacheMatrix
  my_matrix$get()
  ##Visualizo la matriz original 'x'##
  my_matrix$getInverse()
  ##Visualizo la matriz inversa de 'x'##
  cacheSolve(my_matrix)
  ##Me devuelve el valor de la matriz inversa guardada en el caché##
}

############################################
##2. Con numeros ingresados por el usuario##
############################################
print('Ingrese el número de columnas de su matriz cuadrada en la consola y aplaste ENTER')
n<-scan(file = "",what = numeric(),n = 1)
print('Ingrese los valores por columna en la consola y aplaste ENTER')
print('El programa solo va a leer los 9 primeros valores que ingrese')
matriZ1<-scan(file = "",what = numeric(),n = n*n)
matriZ1<-matrix(matriZ1,n,n)
print('Su matriz cuadrada es la siguiente:')
print(matriZ1)
class(matriZ1)
dm<-det(matriZ1)
if (dm==0){
          print("No existe la inversa de su matriz por que el determinante es 0")
}else{
          print(paste0('El determinante es igual a:', dm))
          my_matrix<- makeCacheMatrix(matriZ1) 
          #Creo un matriz evaluada en la función makeCacheMatrix
          my_matrix$get()
          ##Visualizo la matriz original 'x'##
          my_matrix$getInverse()
          ##Visualizo la matriz inversa de 'x'##
          cacheSolve(my_matrix)
          ##Me devuelve el valor de la matriz inversa guardada en el caché##
      }

