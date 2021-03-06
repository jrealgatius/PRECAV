library(rmarkdown)

#### fer corre tota la sintaxi

source(here::here("sintaxis","global_lectura.R"))
funcions_lectura_dades(mostra=F)  # Lectura de 400 000 usuaris / o tota la Població

# 1. 
source(here::here("sintaxis","1.LECTURA.R"))

# 2.
rm(params)
rmarkdown::render(here::here("sintaxis","2.PREPARACIO.Rmd"),
                  params=list(evaluar=T,fitxer_entrada="BD_PRECAV_test6.rds",filtre="c_inclusio_grup_MACE>=0"))



# 3. 
rmarkdown::render(here::here("sintaxis","3.RESULTATS.Rmd"), output_file="Resultats_poblacionals")












