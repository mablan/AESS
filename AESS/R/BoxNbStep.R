#' Graficos de caja que comparan  numero de pasos, recursos y actores
#'
#' @description Esta funcion realiza tres graficos de caja o "box plot"
#'              paralelos para comparar el numero de pasos en las corrida, el
#'              estado de los recursos y una de las variables que caracterizan a
#'              los actores (satisfaccion, influencia u objetivo).
#'
#' @param relation la lista con la salidas del modelo.
#' @param variable la variable a analizar.
#' @param perp un valor entero que controla la orientacion en los ejes (0:
#'        paralelo a los ejes, 1: horizontal, 2: perpendicular a los ejes,
#'        3: vertical).
#' @param tam factor de amplificacion o disminucion del tamano del texto en los
#'        ejes.
#' @return Muestra tres graficos de caja: el primero con el numero de pasos en
#'         cada corrida, el segundo con los estados de los recursos y el tercero
#'         con la variable de interes de los actores (satisfaccion, influencia u
#'         objetivo). Los graficos de caja permiten resumir de manera visual
#'         estadisticos descriptivos basicos como tendencia central, dispersion
#'          y valores aberrantes o extremos.
#' @author Maria Morales
#' @references Chambers, J. M., Cleveland, W. S., Kleiner, B. and Tukey,
#'             P. A.(1983) Graphical Methods for Data Analysis.
#'             Wadsworth & Brooks/Cole
#' @seealso boxplot.stats, bxp
#' @examples
#' data(potatoes)
#' boxNbStep(potatoes, "satisfaction")
#' @export
#' @importFrom graphics boxplot layout


boxNbStep<-function(relation, variable=c("resources","satisfaction","influence",
                    "aim"), perp=2, tam=0.6)
{
  par(mfrow=c(1,3))
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
  mat=matrix(c(1,2,3),1,byrow=T)
  layout(mat,c(1.5,3,3))

  boxplot(relation$step,main="nb step")
  boxplot(relation$resources,names=relation[[1]],main="resource states",
          las=perp,cex.axis=tam)
  boxplot(as.data.frame(relation[as.character(tipo)])[,1:n],las=perp,
          cex.axis=tam,main=tipo,names=temp)
}
