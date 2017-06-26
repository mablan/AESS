#' Analisis de componentes principales para los diferentes individuos
#'
#' @description Esta funcion muestra un grafico de la ubicacion de los recursos
#'              o actores con respecto a los dos ejes dominantes de componentes
#'              principales. La variable puede ser estado de los recursos
#'              (resources) o una de las variables caracteristicas de los
#'              actores: satisfaccion (satisfaction), influencia (influence) u
#'              objetivo (aim).
#'
#' @param relation la lista con la salidas del modelo.
#' @param variable la variable a analizar.
#' @return Muestra un grafico de la variable (recursos, satisfaccion, influencia
#'          u objetivo) que se seleccione para el estudio.
#'
#' @author Maria Morales
#' @references Mardia, K. V., J. T. Kent and J. M. Bibby (1979). Multivariate
#'             Analysis, London: Academic Press.
#' @seealso pca, prcomp, ggplot
#' @examples
#' data(potatoes)
#' analysisComponents(potatoes, "satisfaction")
#' @export
#' @importFrom graphics par plot
#' @importFrom FactoMineR PCA
#'

analysisComponents<-function(relation, variable=c("resources","satisfaction",
                                                  "influence","aim"))
{
  par(mfrow=c(1,1))
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

    marco=(as.data.frame(relation[as.character(tipo)])[,1:n])
    names(marco) =temp
    FactoMineR::PCA(marco,scale.unit=TRUE, axes = c(1,2))
}

