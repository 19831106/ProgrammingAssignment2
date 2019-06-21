## Put comments here that give an overall description of what your
## functions do

#############################################################################################
# La primera función, makecache crea una matriz, teniendo en cuenta los siguientes pasos:   #
# 1. establecer el valor de la matriz                                                       # 
# 2. obtener el valor de la matriz                                                          #
# 3. establecer la inversa de la matriz                                                     #
# 4. obtener el valor de la inversa de la matriz                                            #
#############################################################################################
a=1:12
x=3
makecache<- function(){
  matrix(a,nrow = x,byrow = TRUE)
}
  get<-function()a
  setinversa<-apply(a,FUN = solve,MARGIN = c(1,2))
  getinversa<-function()a
  
###############################################################################################
# La siguiente función calcula la inversa de la matriz creada con la función anterior.        #
#  Sin embargo, primero verifica si la media ya ha sido calculada.                            # 
#  en caso afirmativo, obtiene la inversa del caché y omite el cálculo.                       #
#  De lo contrario,                                                                           #
#  calcula y establece la inversa de la matriz en el caché a través de la función setinversa  #
###############################################################################################  

  cacheinversa <- function(a,solve) {
    m <- a$getinversa()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- a$get()
    m <- inversa(data, ...)
    a$setinversa(m)
    m
  }  
