% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ClusActResources.R
\name{clusActResources}
\alias{clusActResources}
\title{Analisis de conglomerados o "cluster" para las corridas de SocLab}
\usage{
clusActResources(relation, variable = c("resources", "satisfaction",
  "influence", "aim"), colors = "Blues", cf = 3, cc = 3, xaxis = "8pt",
  yaxis = "5pt")
}
\arguments{
\item{relation}{la lista con la salidas del modelo.}

\item{variable}{la variable a analizar.}

\item{colors}{indica la paleta de colores a utilizar en el mapa de calor. Por
defecto azules.}

\item{cf}{numero de grupos o colores diferentes para identificar los grupos
conformados en las filas del mapa de calor.}

\item{cc}{numero de grupos o colores diferentes para identificar los grupos
conformados en las columnas del mapa de calor.}

\item{xaxis}{tamano de la fuente de etiquetas en el eje x en puntos y entre
comillas. El valor por defecto es "8pt".}

\item{yaxis}{tamano de la fuente de etiquetas en el eje y en puntos y entre
comillas. El valor por defecto es "5pt".}
}
\value{
Muestra un mapa de calor que utilizando el metodo de distancia
        euclidiana permite visualizar la similtud o disparidad entre actores
        o recursos.
}
\description{
Esta funcion realiza un analisis de conglomerados que permite
             mostrar de manera grafica la agrupacion o asociacion de las
             corridas con respecto a una de las variables caracteristicas de
             los actores (satisfaccion, influencia u objetivo) o de los
             recursos mediante el uso de un mapa de calor.
}
\note{
El interfaz fue disenado basado en heatmap y heatmap.2
}
\examples{
data(potatoes)
clusActResources(potatoes, "satisfaction")
}
\author{
Maria Morales
}
\references{
https://blog.rstudio.org/2015/06/24/d3heatmap/
}
\seealso{
heatmap, heatmap.2
}

