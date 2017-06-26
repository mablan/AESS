#' Analisis de Correlacion para las diferentes variables
#'
#' @description Esta funcion permite observar la correlacion que existe entre
#'              las diferentes variables que caracterizan a los actores
#'              (satisfaccion, influencia u objetivo) y el estado de los
#'              recursos, con respecto al numero de pasos.
#'
#' @param relation la lista con la salidas del modelo.
#' @param variable la variable a analizar.
#' @param colors define un vector de colores para la grafica.
#' @param cexl  factor de amplificacion o disminucion del tamano del texto en
#'        los ejes.
#' @return Muestra un grafico para cada una de las variables (satisfaccion,
#'         influencia, objetivo y recursos) y una tabla donde se
#'         observa el indice de correlacion de las diferentes variables en
#'         estudio.
#' @author Maria Morales
#' @references Murdoch, D.J. and Chow, E.D. (1996). A graphical display of large
#'             correlation matrices. The American Statistician 50, 178-180.
#' @seealso ellipse
#' @examples
#' data(potatoes)
#' correlationActors(potatoes, "satisfaction")
#' @export
#' @importFrom ellipse plotcorr
correlationActors<-function(relation, variable=c("resources","satisfaction",
                            "influence","aim"), colors=(c("gray","black")),
                            cexl=0.6)
{
  par(mfrow=c(1,1))
  nb_step<-c(relation$step)
  tipo=match.arg(variable)
  if (tipo=="resources")
  {
    dat<-cbind(nb_step,relation$resources)
    temp= relation$ResName
    names(dat)=c("nb_step",temp)
  }
  else if(tipo=="satisfaction")
  {
    dat<-cbind(nb_step,relation$satisfaction)
    temp= relation$ActName
    names(dat)=c("nb_step",temp)
  }
  else if(tipo=="influence")
  {
    dat<-cbind(nb_step,relation$influence)
    temp= relation$ActName
    names(dat)=c("nb_step",temp)
  }
  else if(tipo=="aim")
  {
    dat<-cbind(nb_step,relation$aim)
    temp= relation$ActName
    names(dat)=c("nb_step",temp)
  }
  print(cor(dat))
  ellipse::plotcorr(cor(dat), col=colors, main=paste("Correlation of ", tipo,
                    collapse= ", "),cex.lab=cexl)
}
