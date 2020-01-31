library(rmarkdown)

code_dir<-"sintaxis"
report_filename<-"EDICIO_PRECAV_V3.Rmd"
report_filename <- file.path(code_dir, report_filename)

output_dir <- "output/reports"
output <- file.path("..",output_dir)
render(report_filename, output_dir = output_dir, params = list(output_dir = output))




#### fer corre tota la sintaxi
funcions_lectura_dades(mostra=T)  # Lectura de 400 000 usuaris / o tota la Població

source(here::here("sintaxis","1.LECTURA.R"))
rmarkdown::render(here::here("sintaxis","2.PREPARACIO.Rmd"))
rmarkdown::render(here::here("sintaxis","3.RESULTATS.Rmd"))





