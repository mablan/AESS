\name{potatoes}
\alias{potatoes}
\docType{data}
\title{
Potatoes Data Set
}
\description{
Contiene los resultados de salida de SocLab para una serie de corridas del modelo de los productores de semilla de papa.
}
\usage{potatoes}
\format{
 Es una lista con 7 componentes:
 \itemize{
    \item ResName: vector que contiene los nombres de los recursos utilizados en el modelo.
    \item ActName: vector que contiene los nombres de los actores utilizados en el modelo.
    \item resources: marco de datos con el estado de los recursos para las 100 corridas del modelo.
    \item satisfaction: marco de datos con la variable satisfaccion de cada uno de los actores para las 100 corridas del modelo.
    \item influence: marco de datos con la variable influencia de cada uno de los actores para las 100 corridas del modelo.
    \item aim: marco de datos con la variable meta de cada uno de los actores para las 100 corridas del modelo.
    \item step:vector con el numero de pasos en cada corrida.
  }
}

\details{
Los resultados de todas las corridas en SocLab siempre se importaran en R siguiendo esta estructura. La funcion: \link{dataSocLab} se encarga de leer los datos texto de salida de la corrida de SocLab y colocarlos en este formato
}
\source{
Teran, Oswaldo R., Rojas Ravi, Romero Liccia, Sibertin-Blanc Christhophe, Alarcon Lisbeth.(2015, November 22) \href{https://www.openabm.org/model/4606/version/5}{Model of the social game associated to the production of potato seeds in a Venezuelan region" (Version 5).}
}

\examples{
data(potatoes)
## Ver la estructura de los datos
str(potatoes)

## Obtener un resumen estadistico del estado de los recursos:
summary(potatoes$resources)
}
\keyword{datasets}
