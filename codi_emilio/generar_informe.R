library(rmarkdown)

#### fer corre tota la sintaxi

# 1.
rmarkdown::render(here::here("codi_emilio","1_lectura_Eortega.Rmd"),
                  params=list(mostra= F))



# 2.

rmarkdown::render(here::here("codi_emilio","2_preparacio_emilio.Rmd"),
                  params=list(mostra= T),
                  output_file=here::here("codi_emilio/resultats",paste0("Resultats_",Sys.Date())))


