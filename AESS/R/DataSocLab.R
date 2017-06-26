#' Lectura del archivo de salida de SocLab
#'
#' Esta funcion es para leer los datos de salida de SocLab. Se presume que los
#'      datos estan en un formato texto separado por tabulaciones. Si los datos
#'      no se encuentran en el directorio de trabajo hay que especificar el
#'      camino completo. Este es el primer paso para cualquier analisis
#'      posterior.
#'
#' @param direction_file es una cadena de caracteres que contiene el nombre o el
#'        camino (directorio + nombre) del archivo de entrada, donde esta el
#'        archivo con los datos de salida de SocLab.
#'
#' @return Retorna una lista  que contiene:
#'
#'  ResName: vector que contiene los nombres de los recursos utilizados en el
#'           modelo.
#'
#'  ActName: vector que contiene los nombres de los actores utilizados en el
#'           modelo.
#'
#'  resources: marco de datos con el estado de los recursos para todas las
#'             corridas del modelo.
#'
#'  satisfaction: marco de datos con la variable satisfaccion de cada uno de los
#'                actores para todas las corridas del modelo.
#'
#'  influence: marco de datos con la variable influencia de cada uno de los
#'             actores para todas las corridas del modelo.
#'
#'  aim: marco de datos con la variable objetivo de cada uno de los actores para
#'       todas las corridas del modelo.
#'
#'  step:vector con el numero de pasos en cada corrida.

#' @examples
#'  ## Un ejemplo del archivo de salida de SocLab es potatoesModelV4.txt que se
#'  ## encuentra en el directorio extdata del directorio de instalacion del
#'  ## paquete AESS
#'  ## La siguiente instruccion devuelve como una cadena de caracteres la
#'  ## direccion (camino + nombre) de este archivo
#'  nombre.arc=system.file("extdata", "potatoesModelV4.txt", package = "AESS")
#'  ## La entrada a la funcion es la direccion donde se encuentra el archivo
#'  datos<-dataSocLab(nombre.arc)
#'  summary(datos)
#' @export
#' @importFrom utils read.table

dataSocLab<-function(direction_file)
{
  x<- read.table(direction_file, header=TRUE, fill=TRUE, sep= "\t")

  #Nombres
  nombres=names(x)

  #n es el numero de actores
  i_satis=grep(".Satisfaction",nombres)
  n=length(i_satis)

  #m es el numero de recursos
  i_recur=grep(".State",nombres)
  m=length(i_recur)

  #Nombre de los recursos.
  recur =x[,i_recur]
  puntoState = regexpr(".State",nombres[i_recur])
  nam_recur=substr(nombres[i_recur],start=1,stop=puntoState-1)

  #Nombre de los actores que pertenecen a la variable Satisfaccion.
  i_satis=grep(".Satisfaction",nombres)
  satis =x[,i_satis]
  puntoSatis=regexpr(".Satisfaction",nombres[i_satis])
  nam_actores=substr(nombres[i_satis],start=1,stop=puntoSatis-1)

  #Nombre de los actores que pertenecen a la variable Influencia.
  i_influ=grep(".Influence",nombres)
  influ =x[,i_influ]
  puntoInflu=regexpr(".Influence",nombres[i_influ])
  nam_actores=substr(nombres[i_influ],start=1,stop=puntoInflu-1)

  #Nombre de los actores que pertenecen a la variable Objetivo.
  i_aim=grep(".Aim",nombres)
  aim =x[,i_aim]
  puntoAim=regexpr(".Aim",nombres[i_aim])
  nam_actores=substr(nombres[i_aim],start=1,stop=puntoAim-1)

  #Esta funcion retorna el nombre de los recursos, nombre de los actores,las
  #variables recursos, satisfacciOn, influencia y objetivo y el nUmero de pasos
  ResName <- nam_recur
  ActName <- nam_actores
  ResNum <- m
  ActNum <- n

  cat("Hay ",m,"recursos y sus nombres son: ", ResName,"\n")
  cat("Hay ",n,"actores y sus nombres son: ", ActName,"\n")

  #Esta funcion retorna el nombre de los recursos, nombre de los actores,las
  #variables recursos, satisfacciOn, influencia y objetivo y el nUmero de pasos.
  return(list(ResName=nam_recur, ActName=nam_actores, resources=recur,
              satisfaction=satis, influence=influ, aim=aim, step=x$nb_step))
}
