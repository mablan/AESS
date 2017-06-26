#' Analisis de conglomerados o "cluster" para las corridas de SocLab
#'
#' @description Esta funcion realiza un analisis de conglomerados que permite
#'              mostrar de manera grafica la agrupacion o asociacion de las
#'              corridas con respecto a una de las variables caracteristicas de
#'              los actores (satisfaccion, influencia u objetivo) o de los
#'              recursos mediante el uso de un mapa de calor.
#'
#' @param relation la lista con la salidas del modelo.
#' @param variable la variable a analizar.
#' @param colors indica la paleta de colores a utilizar en el mapa de calor. Por
#'        defecto azules.
#' @param cf numero de grupos o colores diferentes para identificar los grupos
#'        conformados en las filas del mapa de calor.
#' @param cc numero de grupos o colores diferentes para identificar los grupos
#'        conformados en las columnas del mapa de calor.
#' @param xaxis tamano de la fuente de etiquetas en el eje x en puntos y entre
#'        comillas. El valor por defecto es "8pt".
#' @param yaxis tamano de la fuente de etiquetas en el eje y en puntos y entre
#'        comillas. El valor por defecto es "5pt".
#'
#' @return Muestra un mapa de calor que utilizando el metodo de distancia
#'         euclidiana permite visualizar la similtud o disparidad entre actores
#'         o recursos.
#' @author Maria Morales
#' @references https://blog.rstudio.org/2015/06/24/d3heatmap/
#' @seealso heatmap, heatmap.2
#' @note El interfaz fue disenado basado en heatmap y heatmap.2
#' @examples
#' data(potatoes)
#' clusActResources(potatoes, "satisfaction")
#' @export
#' @importFrom graphics boxplot layout
#' @importFrom stats dist
#' @importFrom d3heatmap d3heatmap

clusActResources<-function(relation, variable=c("resources", "satisfaction",
                           "influence","aim"), colors = "Blues", cf=3,
                           cc=3, xaxis="8pt", yaxis="5pt")
{
  par(mfrow=c(1,1))
  tipo=match.arg(variable)
  if (tipo=="resources")
  {
    cl=relation$resources
    temp= relation$ResName
    names(cl)=temp
  }
  else if (tipo=="satisfaction")
  {
    cl=relation$satisfaction
    temp= relation$ActName
    names(cl)=temp
  }
  else if (tipo=="influence")
  {
    cl=relation$influence
    temp= relation$ActName
    names(cl)=temp
  }
  else if (tipo=="aim")
  {
    cl=relation$aim
    temp= relation$ActName
    names(cl)=temp
  }

  #Metodo de la distancia euclideana
  distancias<-dist(cl, method="euclidean")

  d3heatmap::d3heatmap(as.matrix(cl), colors=colors, Rowv=TRUE, scale="column",
                       k_row=cf, k_col=cc,xaxis_font_size=xaxis,
                       yaxis_font_size=yaxis)
}
