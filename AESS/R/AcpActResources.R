#' Analisis de componentes principales para las diferentes variables
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
#'          u objetivo) que se seleccione para el estudio y una tabla donde
#'          se puede observar la desviacion estandar, proporcion de la varianza
#'          explicada y la varianza acumulada para cada componente. Ademas
#'          origina una tabla donde se puede observar la desviacion estandar,
#'          proporcion de la varianza explicada por los componentes y la
#'          varianza acumulada.
#' @author Maria Morales
#' @references Mardia, K. V., J. T. Kent and J. M. Bibby (1979). Multivariate
#'             Analysis, London: Academic Press.
#' @seealso pca, prcomp, ggplot
#' @examples
#' data(potatoes)
#' acpActResources(potatoes, "satisfaction")
#' @export
#' @importFrom graphics par
#' @importFrom stats prcomp cor
#' @importFrom ggplot2 ggplot geom_path aes geom_text geom_hline geom_vline labs
#'             geom_segment ggtitle xlim ylim
acpActResources <- function(relation, variable=c("resources","satisfaction",
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

  # function to create a circle
  circle <- function(center=c(0,0), npoints=100)
  {
    r = 1
    tt = seq(0, 2*pi, length=npoints)
    xx = center[1] + r * cos(tt)
    yy = center[1] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }

  pca = prcomp(as.data.frame(relation[as.character(tipo)])[,1:n], scale. =TRUE)
  print(summary(pca))
  corcir = circle(c(0,0), npoints = 100)

  # create data frame with correlations between variables and PCs
  correlations = as.data.frame(cor(as.data.frame(relation[as.character(tipo)])
                                   [,1:n], pca$x))

  x1=c(0)
  y1=c(0)
  x2=TRUE
  y2=TRUE
  x=TRUE
  y=TRUE
  PC1=TRUE
  PC2=TRUE
  # data frame with arrows coordinates
  arrows = data.frame(x1, y1, x2=correlations$PC1, y2=correlations$PC2)

  ggplot()+geom_path(data=corcir, aes(x=x, y=y), colour="gray65")+
    geom_segment(data=arrows, aes(x=x1, y=y1, xend=x2, yend=y2), colour="red")+
    geom_text(data=correlations, aes(x=PC1, y=PC2, label=temp))+
    geom_hline(yintercept=0,colour="black") +geom_vline(xintercept=0,
    colour="black") +xlim(-1.1,1.1) + ylim(-1.1,1.1) +labs(x="pc1 axis",
    y="pc2 axis")+ ggtitle(variable)
}
