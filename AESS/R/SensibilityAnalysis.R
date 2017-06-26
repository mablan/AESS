#' Analisis de sensibilidad para las diferentes variables
#'
#' @description Esta funcion permite determinar que tan sensible puede llegar a
#'              ser el modelo ante cambios realizados en los valores de los
#'              parametros.
#'
#' @param lp la lista con la salidas del modelo.
#' @param variable la variable a analizar.
#' @param opcion una variable que almacena una cadena de caracteres. Esta cadena
#'        de caracteres contiene: el parametro (param0), el numero de
#'        pasos (nb_step) y el numero de experimentos (num_exp) respectivamente.
#' @param j un valor que define la posicion de la leyenda en el eje x.
#' @param k un valor que define la posicion de la leyenda en el eje y.
#' @return Muestra un grafico de la variable que caracterizan a los actores
#'         (satisfaccion, influencia u objetivo) y la variable recursos que
#'         controla cada actor, asi como tambien un grafico para el nb_step y
#'         num_exp.
#' @author Maria Morales
#' @examples
#' data(parameters)
#' sensibilityAnalysis(parameters, "satis", "param0")
#' @export
#' @importFrom graphics plot legend lines points

sensibilityAnalysis<-function(lp, variable=c("satis", "inf", "aim", "staterel"),
                opcion=c("param0", "nb_step", "num_exp"), j=0.5, k=35)
{
  #tipo1=match.arg(opcion)
  if(opcion=="param0")
  {
    value=(cbind(lp$param0))
    name= lp$nombre
    print(name)
  }
  else if(opcion=="nb_step")
  {
    value=(cbind(lp$nb_step))
    name="nb_step"
  }
  else if(opcion=="num_exp")
  {
    value=(cbind(lp$num_exp))
    name="num_exp"
  }
  x=sort(value)
  tipo=match.arg(variable)
  if (tipo=="staterel")
  {
    y=lp$staterel
    titulo="Analysis of sensibility for the variable Resources";
    etiquetaY="Resources";
  }
  else if(tipo=="satis")
  {
    y=lp$satis
    titulo="Analysis of sensibility for the variable Satisfaction";
    etiquetaY="Satisfaction";
  }
  else if(tipo=="inf")
  {
    y=lp$inf
    titulo="Analysis of sensibility for the variable Influence";
    etiquetaY="Influence";
  }
  else if(tipo=="aim")
  {
    y=lp$aim
    titulo="Analysis of sensibility for the variable Aim";
    etiquetaY="Aim";
  }
  par(mfrow=c(1,1))
  plot(x, y[,1], type="p", main=titulo, xlab=name, ylab=etiquetaY, c(min(x),
                            max(x)), c(min(y), max(y)))
  numColor<-c()

  for(i in 1:length(lp$staterel))
  {
    n=length(lp[[1]])
    s1= lp$ResName
    lines(x,y[,i],type="l",col=i)
    points(x,y[,i],type="p", col=i)
    numColor[i]<-i
  }
  legend(x=j, y=k, legend=c(lp$ResName), lty=c(1), col=c(numColor), cex=c(0.5),
         lwd=2)

  for(i in 1:length(lp$satis))
  {
    n=length(lp[[2]])
    s2= lp$ActName
    lines(x,y[,i],type="l",col=i)
    points(x,y[,i],type="p", col=i)
    numColor[i]<-i
  }
  legend(x=j, y=k, legend=c(lp$ActName), lty=c(1), col=c(numColor), cex=c(0.5),
         lwd=2)
}

