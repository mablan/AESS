\name{potatoesSen}
\alias{potatoesSen}
\docType{data}
\title{
PotatoesSen Data Set
}
\description{
Contiene los resultados del analisis de sensibilidad de salida de SocLab para el parametro identificacion de los actores del modelo de los productores de semilla de papa.
}
\usage{potatoesSen}
\format{
 Es una lista con 9 componentes:
 \itemize{
    \item ResName: vector que contiene los nombres de los recursos utilizados en el modelo.
    \item ActName: vector que contiene los nombres de los actores utilizados en el modelo.
    \item staterel: marco de datos con el estado de los recursos para las 5 corridas del modelo.
    \item satis: marco de datos con la variable satisfaccion de cada uno de los actores para las 5 corridas del modelo.
    \item inf: marco de datos con la variable influencia de cada uno de los actores para las 5 corridas del modelo.
    \item aim: marco de datos con la variable meta de cada uno de los actores para las 5 corridas del modelo.
    \item param0: vector con la variacion de los parametros en cada corrida
    \item nb_step: vector con el numero de pasos en cada corrida.
    \item parameters: parametro a variar.
  }
}

\details{
Los resultados de todas las corridas en SocLab siempre se importaran en R siguiendo esta estructura. La funcion: \link{dataSocLab1} se encarga de leer los datos texto de salida de la corrida de SocLab y colocarlos en este formato
}
\source{
Teran, Oswaldo R., Rojas Ravi, Romero Liccia, Sibertin-Blanc Christhophe, Alarcon Lisbeth.(2015, November 22) \href{https://www.openabm.org/model/4606/version/5}{Model of the social game associated to the production of potato seeds in a Venezuelan region" (Version 5).}
}

\examples{
data(potatoesSen)
## Ver la estructura de los datos
str(potatoesSen)
}
\keyword{datasets}
