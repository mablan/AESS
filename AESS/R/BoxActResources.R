#' Graficos de caja para las variables caracteristicas de los actores o recursos
#'
#' @description Esta funcion realiza un grafico de caja o "box plot" para las
#'              variables que caracterizan a los actores (satisfaccion,
#'              influencia u objetivo) o el estado de los recursos.
#'
#' @param relation la lista con la salidas del modelo.
#' @param variable la variable a analizar.
#' @param perp un valor entero que controla la orientacion en los ejes (0:
#'        paralelo a los ejes, 1: horizontal, 2: perpendicular a los ejes,
#'        3: vertical).
#' @param tam factor de amplificacion o disminucion del tamano del texto en los
#'        ejes.
#' @return Muestra un grafico de caja para la variable seleccionada (recursos,
#'         satisfaccion, influencia u objetivo). Los graficos de caja permiten
#'         resumir de manera visual estadisticos descriptivos basicos como
#'         tendencia central, dispersion y valores aberrantes o extremos.
#' @author Maria Morales
#' @references Chambers, J. M., Cleveland, W. S., Kleiner, B. and Tukey,
#'             P. A.(1983) Graphical Methods for Data Analysis.
#'             Wadsworth & Brooks/Cole
#' @seealso boxplot.stats, bxp
#' @examples
#' data(potatoes)
#' boxActResources(potatoes, "satisfaction")
#' @export
#' @importFrom graphics boxplot
boxActResources<-function(relation, variable=c("resources","satisfaction",
                          "influence","aim"),perp=2, tam=0.6)
{
  par(mfrow=c(1,1))
  tipo=match.arg(variable)

  #Se declara una variable temp donde se guardaran el nombre de los recursos y
  #nombre de los actores.
  if (tipo=="resources")
  {
    n=length(relation[[1]])
    temp= relation$ResNam
  }
  else
  {
    n=length(relation[[2]])
    temp= relation$ActName
  }

  boxplot(as.data.frame(relation[as.character(tipo)])[,1:n],las=perp,
          cex.axis=tam,main=tipo,names=temp)

}
