#' Lectura del archivo de salida de SocLab para el analisis de sensibilidad
#'
#' Esta funcion es para leer los datos de salida de SocLab. Se presume que los
#'      datos estan en un formato texto separado por tabulaciones. Si los datos
#'      no se encuentran en el directorio de trabajo hay que especificar el
#'      camino completo.
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
#'  staterel: marco de datos con el estado de los recursos para todas las
#'             corridas del modelo.
#'
#'  satis: marco de datos con la variable satisfaccion de cada uno de los
#'                actores para todas las corridas del modelo.
#'
#'  inf: marco de datos con la variable influencia de cada uno de los
#'             actores para todas las corridas del modelo.
#'
#'  aim: marco de datos con la variable objetivo de cada uno de los actores para
#'       todas las corridas del modelo.
#'
#'  param0: vector con la variacion de los parametros en cada corrida.
#'
#'  nb_step: vector con el numero de pasos en cada corrida.
#'
#'  num_exp: vector con el numero de experimentos en cada corrida.
#'
#'  parameters: parametro a variar.
#'
#' @examples
#'  ## Un ejemplo del archivo de salida de SocLab es
#'  ## sensitivityPotatoesModelV4.txt que se
#'  ## encuentra en el directorio extdata del directorio de instalacion del
#'  ## paquete AESS
#'  ## La siguiente instruccion devuelve como una cadena de caracteres la
#'  ## direccion (camino + nombre) de este archivo
#'  nombre.arc=system.file("extdata", "sensitivityPotatoesModelV4.txt",
#'  package = "AESS")
#'  ## La entrada a la funcion es la direccion donde se encuentra el archivo
#'  datos<-dataSocLab1(nombre.arc)
#' @export
#' @importFrom utils read.table

dataSocLab1<-function(direction_file)
{
  datos<- read.table(direction_file, header=TRUE, skip=5, fill=TRUE, sep= "\t")
  dat<-readLines(direction_file,4,skipNul = TRUE)
  prueba=readLines(direction_file,4,skipNul = TRUE)
  tabuladores=gregexpr("\t",prueba[4])
  inicio=tabuladores[[1]][1]+1
  fin=tabuladores[[1]][2]-1
  nomb=substr(prueba[4],inicio,fin)

  nombres=names(datos)

  #n es el numero de actores
  i_satisfaction=grep("_satis",nombres)
  n=length(i_satisfaction)

  #m es el numero de recursos
  i_recur=grep("_staterel",nombres)
  m=length(i_recur)

  #Nombre de los recursos.
  recur =datos[,i_recur]
  pisostaterel = regexpr("_staterel",nombres[i_recur])
  nam_recur=substr(nombres[i_recur],start=1,stop=pisostaterel-1)

  #Nombre de los actores que pertenecen a la variable Satisfaccion.
  i_satisfaction=grep("_satis",nombres)
  satisfaction =datos[,i_satisfaction]
  pisoSatis=regexpr("_satis",nombres[i_satisfaction])
  nam_actores=substr(nombres[i_satisfaction],start=1,stop=pisoSatis-1)

  #Nombre de los actores que pertenecen a la variable Influencia.
  i_influence=grep("_inf",nombres)
  influence =datos[,i_influence]
  pisoInflu=regexpr("_inf",nombres[i_influence])
  nam_actores=substr(nombres[i_influence],start=1,stop=pisoInflu-1)

  #Nombre de los actores que pertenecen a la variable Influencia.
  i_aim=grep("_aim",nombres)
  aim =datos[,i_aim]
  pisoAim=regexpr("_aim",nombres[i_aim])
  nam_actores=substr(nombres[i_aim],start=1,stop=pisoAim-1)

  #Esta funcion retorna el nombre de los actores,las variables satisfaccion,
  #influencia, objetivo, recurso, los parametros, el numero de pasos y el numero
  #de experimentos.
  ResName <- nam_recur
  ActName <- nam_actores
  ResNum <- m
  ActNum <- n

  cat("Hay ",m,"recursos y sus nombres son: ", ResName,"\n")
  cat("Hay ",n,"actores y sus nombres son: ", ActName,"\n")
  return(list(ResName=nam_recur,ActName=nam_actores, satis=satisfaction,
              inf=influence, aim=aim, param0=datos$param0, staterel=recur,
            nb_step=datos$nb_step, num_exp=datos$num_exp, nombre=nomb))
}

