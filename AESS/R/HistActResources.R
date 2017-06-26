#' Graficos de histogramas para cada una de las variables
#'
#' @description Esta funcion realiza histogramas paralelos de los resultados de
#'              las corridas para las variables que caracterizan a los actores
#'              (satisfaccion, influencia u objetivo) o para los diferentes
#'              recursos.
#'
#' @param relation la lista con la salidas del modelo.
#' @param variable la variable a analizar.
#' @param nrow indica el numero de filas en que es divida la pantalla para
#'       visualizar las graficas.
#' @param ncol indica el numero de columnas en que es divida la pantalla para
#'       visualizar las graficas.
#' @param colors Color seleccionado para la grafica.
#' @return Muestra un conjunto de histogramas para cada una de las variables.
#' @author Maria Morales
#' @references Chambers, J. M., Cleveland, W. S., Kleiner, B. and Tukey,
#'             A. (1983) Graphical Methods for Data Analysis.
#'             Wadsworth & Brooks/Cole
#' @seealso barplot
#' @note Se debe seleccionar el tamano de la columna y fila adecuado para que
#'       las graficas se visualicen mejor.
#' @examples
#'    data(potatoes)
#'    histActResources(potatoes, "satisfaction")
#' @export
#' @importFrom graphics hist
histActResources<-function(relation, variable=c("resources","satisfaction",
                  "influence","aim"), nrow=3,ncol=2,  colors="purple")
{
  par(mfrow=c(nrow,ncol))
  tipo=match.arg(variable)
  if (tipo=="resources")
  {
    n=length(relation[[1]])
    temp= relation$ResName
  }
  else
  {
    n=length(relation[[2]])
    temp= relation$ActName
  }
  for(i in 1:n)
  {
    hist(as.data.frame(relation[as.character(tipo)])[,i], main=temp[i],
         xlab= tipo, col=colors)
  }
}
