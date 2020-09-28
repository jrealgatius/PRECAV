library(rmarkdown)

#### fer corre tota la sintaxi

# 1.
rmarkdown::render(here::here("codi_emilio","1_lectura_Eortega.Rmd"),
                  params=list(mostra= FALSE))



# 2.
rmarkdown::render(here::here("codi_emilio","2_preparacio_emilio.Rmd"),
                  params=list(mostra= FALSE),
                  output_file=here::here("codi_emilio/resultats","Resultats_poblacionals2"))


