% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HistActResources.R
\name{histActResources}
\alias{histActResources}
\title{Graficos de histogramas para cada una de las variables}
\usage{
histActResources(relation, variable = c("resources", "satisfaction",
  "influence", "aim"), nrow = 3, ncol = 2, colors = "purple")
}
\arguments{
\item{relation}{la lista con la salidas del modelo.}

\item{variable}{la variable a analizar.}

\item{nrow}{indica el numero de filas en que es divida la pantalla para
visualizar las graficas.}

\item{ncol}{indica el numero de columnas en que es divida la pantalla para
visualizar las graficas.}

\item{colors}{Color seleccionado para la grafica.}
}
\value{
Muestra un conjunto de histogramas para cada una de las variables.
}
\description{
Esta funcion realiza histogramas paralelos de los resultados de
             las corridas para las variables que caracterizan a los actores
             (satisfaccion, influencia u objetivo) o para los diferentes
             recursos.
}
\note{
Se debe seleccionar el tamano de la columna y fila adecuado para que
      las graficas se visualicen mejor.
}
\examples{
   data(potatoes)
   histActResources(potatoes, "satisfaction")
}
\author{
Maria Morales
}
\references{
Chambers, J. M., Cleveland, W. S., Kleiner, B. and Tukey,
            A. (1983) Graphical Methods for Data Analysis.
            Wadsworth & Brooks/Cole
}
\seealso{
barplot
}

