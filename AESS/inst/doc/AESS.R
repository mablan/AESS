## ------------------------------------------------------------------------
library(AESS)
potatoes<-dataSocLab("../inst/extdata/potatoesModelV4.txt")

## ---- fig.height=6, fig.width=6------------------------------------------
histActResources(potatoes, "satisfaction")

## ---- fig.height=6, fig.width=6------------------------------------------
histActResources(potatoes, "resources")

## ---- fig.height=6, fig.width=6------------------------------------------
boxActResources(potatoes, "satisfaction")

## ---- fig.height=6, fig.width=7------------------------------------------
boxNbStep(potatoes, "influence")

## ---- fig.height=5, fig.width=6------------------------------------------
acpActResources(potatoes, "satisfaction")

## ---- fig.height=5, fig.width=6------------------------------------------
acpActResources(potatoes, "resources")

## ---- fig.height=5, fig.width=6------------------------------------------
analysisComponents(potatoes, "satisfaction")

## ---- fig.height=6, fig.width=6------------------------------------------
correlationActors(potatoes, "satisfaction")

## ---- fig.height=6, fig.width=6------------------------------------------
correlationActResources(potatoes, "satisfaction")

## ---- fig.height=6, fig.width=6------------------------------------------
regressionActors(potatoes, "PROINPA_MST", "satisfaction")

## ---- fig.height=6, fig.width=8------------------------------------------
clusActResources(potatoes, "influence")

## ---- fig.height=6, fig.width=8------------------------------------------
clusActResources(potatoes, "resources")

## ------------------------------------------------------------------------
library(AESS)
data(parameters)

## ---- fig.height=6, fig.width=7------------------------------------------
sensibilityAnalysis(parameters, "satis", "param0")

## ---- fig.height=6, fig.width=7------------------------------------------
sensibilityAnalysis(parameters, "inf", "nb_step", j = 11000, k = 35)

## ---- fig.height=6, fig.width=7------------------------------------------
sensibilityAnalysis(parameters, "aim", "num_exp", j = 2, k = 35)

