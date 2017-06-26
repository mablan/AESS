#' Analisis de correlacion entre las variables caracteristicas de los actores y
#' el estado de los recursos
#'
#' @description Esta funcion permite observar la correlacion que existe entre
#'              las variables que caracterizan a los actores (satisfaccion,
#'              influencia u objetivo) con respecto a la variable del estado de
#'              los recursos, tomando en cuenta el numero de pasos.
#'
#' @param relation la lista con la salidas del modelo.
#' @param variable la variable a analizar.
#' @param colors define un vector de colores para la grafica.
#' @param cexl factor de amplificacion o disminucion del tamano del texto en los
#'        ejes.
#' @return Muestra un grafico para cada una de las variables (satisfaccion,
#'         influencia u objetivo) con respecto a la variable de estado de los
#'         recursos.
#' @author Maria Morales
#' @references Murdoch, D.J. and Chow, E.D. (1996). A graphical display of large
#'             correlation matrices. The American Statistician 50, 178-180.
#' @seealso ellipse
#' @examples
#' data(potatoes)
#' correlationActResources(potatoes, "satisfaction")
#' @export
#' @importFrom ellipse plotcorr
correlationActResources<-function(relation, variable=c("satisfaction",
                                  "influence","aim"),colors=(c("gray","black")),
                                  cexl=0.6)
{
  par(mfrow=c(1,1))
  nb_step<-c(relation$step)
  tipo=match.arg(variable)
  if(tipo=="satisfaction")

  #temp1 y temp2 se guardaran los nombres de los actores y recursos
  #respectivamente
  {
    dat<-cbind(nb_step,relation$satisfaction,relation$resources)
    temp1= relation$ActName
    temp2= relation$ResName

    names(dat)=c("nb_step", temp1, temp2)

  }
  else if(tipo=="influence")
  {
    dat<-cbind(nb_step,relation$influence,relation$resources)
    temp1= relation$ActName
    temp2= relation$ResName
    names(dat)=c("nb_step", temp1, temp2)
  }
  else if(tipo=="aim")
  {
    dat<-cbind(nb_step,relation$aim,relation$resources)
    temp1= relation$ActName
    temp2= relation$ResName
    names(dat)=c("nb_step",temp1, temp2)
  }

  ellipse::plotcorr(cor(dat),col=colors, main=paste("Correlation of the
                    relations and", tipo, collapse= ", "), cex.lab=cexl)
}
