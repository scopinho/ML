library(plumber)
library(tidyverse)
library(mgcv)

#* @apiTitle API de identificação de consumo
#* @param hp potencia
#* @param wt peso 
#* @param cyl cilindradas 
#* @post /consumo

function(hp, wt, cyl){
  
  load("modelo.RData")
  
  teste = c(hp=hp, wt=wt, cyl=cyl)
  teste = map_dfr(teste, as.numeric)
  teste$cyl <- factor(teste$cyl, c(4,6,8))
  round(predict(modelo, teste),2)
}