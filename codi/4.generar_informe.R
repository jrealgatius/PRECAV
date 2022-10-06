library(rmarkdown)

#### fer corre tota la sintaxi

# source(here::here("sintaxis","global_lectura.R"))
# funcions_lectura_dades(mostra=F)  # Lectura de 400 000 usuaris / o tota la Població

# 1. 
# source(here::here("sintaxis","1.LECTURA.R"))

rmarkdown::render(here::here("codi","1.LECTURA.Rmd"),
                  params=list(mostra=F))

# 2.
rm(params)
rm(list=ls())
mostra<-F
rmarkdown::render(here::here("codi","2.PREPARACIO.Rmd"),
                  params=list(evaluar=T,
                              fitxer_entrada="BD_PRECAV_test6.rds",
                              filtre="c_inclusio_grup_MACE>=0",
                              mostra=mostra))

# 3. 
rm(list=ls())
gc()

mostra=F
if (mostra) {informe_nom<-paste0("ResultatsCAS_Control_mostra",Sys.Date())} else 
            {informe_nom<-paste0("ResultatsCAS_Control_poblacio",Sys.Date())}

rmarkdown::render(here::here("codi","3.RESULTATS.Rmd"), 
                  output_file=here::here("output/reports",informe_nom),
                  params=list(mostra=mostra))



