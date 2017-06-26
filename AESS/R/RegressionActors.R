#' Analisis de regresion lineal multiple para las variables caracteristicas de
#' los actores
#'
#' @description Esta funcion permite modelar la relacion entre una de las
#'              variables que caracterizan a los actores (satisfaccion,
#'              influencia u objetivo) denominada Y y un vector de variables
#'              explicativas (satisfaccion, influencia u objetivo) denominadas X
#'
#' @param relation la lista con la salidas del modelo.
#' @param varDep la variable dependiente.
#' @param variable la variable a analizar.
#' @return Muestra un conjunto de graficas donde se observa la relacion
#'         entre una variable denominada Y con respecto al vector de variables
#'         explicativas  (satisfaccion, influencia u objetivo). Ademas, las
#'         graficas de diagnostico y una tabla  donde se puede observar algunos
#'         estadisticos como el coeficiente de correlacion de Pearson, el
#'         coeficiente de determinacion R2 ajustado entre otros.
#' @author Maria Morales
#' @references Chambers, J. M. (1992) Linear models. Chapter 4 of Statistical
#'             Models in S eds J. M. Chambers and T. J. Hastie,
#'             Wadsworth & Brooks/Cole.
#'
#'             Wilkinson, G. N. and Rogers, C. E. (1973) Symbolic descriptions
#'             of factorial models for analysis of variance. Applied Statistics.
#' @seealso lm, summary
#' @examples
#' data(potatoes)
#' regressionActors(potatoes, "PROINPA_MST", "satisfaction")
#' @export
#' @importFrom graphics plot
#' @importFrom stats lm
regressionActors<-function(relation, varDep, variable=c("satisfaction",
                          "influence","aim"))
{
  #n es el numero de actores
  n=length(relation[[2]])

  #tipo1, indica la posicion en la que se encuentra el actor
  tipo1=grep(varDep,relation$ActName)
  x= relation$ActName[tipo1]

  tipo2=match.arg(variable)
  if(tipo2=="satisfaction")
  {
    rl= relation$satisfaction

  }
  else if(tipo2=="influence")
  {
    rl= relation$influence
  }
  else if(tipo2=="aim")
  {
    rl= relation$aim
  }
  names(rl)=relation$ActName

  regresion=lm(rl[,x]~.,rl[,-1*tipo1])

  print(summary(regresion))
  par(mfrow=c(2,3))
  for(i in 1:n)
  {
    if(tipo1!=i)
    {
      plot(rl[,x],rl[,i],type="p",xlab=relation$ActName[i],ylab=x)
    }
  }
}
