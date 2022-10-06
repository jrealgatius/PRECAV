library(rmarkdown)

#### fer corre tota la sintaxi

# 1.
rm(list = ls())
gc()

mostra= F

rmarkdown::render(here::here("codi_emilio","1_lectura_Eortega.Rmd"),
                 params=list(mostra= mostra))

# 2.1.  ----------------------    RESULTATS  -------------------
rm(list = ls())
gc()
mostra= F
subgrup= "Overall" # "DM1"  # "Overall" "DM1" "DM2"

#
if (subgrup=="Overall") dir_results<-"codi_emilio/resultats/Overall"
if (subgrup=="DM2") dir_results<-"codi_emilio/resultats/DM2"
if (subgrup=="DM1") dir_results<-"codi_emilio/resultats/DM1"
#

if (mostra) 
  {nom_out<-here::here(dir_results,paste0("ResultatsEPI_mostra_",Sys.Date()))} else 
  {nom_out<-here::here(dir_results,paste0("ResultatsEPI_poblacio_",Sys.Date()))}

rmarkdown::render(here::here("codi_emilio","2_preparacio_emilio.Rmd"),
                  params=list(mostra= mostra,subgrup=subgrup),
                  output_file=nom_out)


# 2.2
rm(list = ls())
gc()
mostra= F
subgrup= "DM2" # "DM1"  # "Overall" "DM1" "DM2"

#
if (subgrup=="Overall") dir_results<-"codi_emilio/resultats/Overall"
if (subgrup=="DM2") dir_results<-"codi_emilio/resultats/DM2"
if (subgrup=="DM1") dir_results<-"codi_emilio/resultats/DM1"
#

if (mostra) 
{nom_out<-here::here(dir_results,paste0("ResultatsEPI_mostra_",Sys.Date()))} else 
{nom_out<-here::here(dir_results,paste0("ResultatsEPI_poblacio_",Sys.Date()))}

rmarkdown::render(here::here("codi_emilio","2_preparacio_emilio.Rmd"),
                  params=list(mostra= mostra,subgrup=subgrup), 
                  output_file=nom_out)


# 2.3
rm(list = ls())
gc()
mostra= F
subgrup= "DM1" # "DM1"  # "Overall" "DM1" "DM2"

#
if (subgrup=="Overall") dir_results<-"codi_emilio/resultats/Overall"
if (subgrup=="DM2") dir_results<-"codi_emilio/resultats/DM2"
if (subgrup=="DM1") dir_results<-"codi_emilio/resultats/DM1"
#

if (mostra) 
{nom_out<-here::here(dir_results,paste0("ResultatsEPI_mostra_",Sys.Date()))} else 
{nom_out<-here::here(dir_results,paste0("ResultatsEPI_poblacio_",Sys.Date()))}

rmarkdown::render(here::here("codi_emilio","2_preparacio_emilio.Rmd"),
                  params=list(mostra= mostra,subgrup=subgrup),
                  output_file=nom_out)


###################

# 2.2
rm(list = ls())
gc()
mostra= T
subgrup= "DM2" # "DM1"  # "Overall" "DM1" "DM2"

#
if (subgrup=="Overall") dir_results<-"codi_emilio/resultats/Overall"
if (subgrup=="DM2") dir_results<-"codi_emilio/resultats/DM2"
if (subgrup=="DM1") dir_results<-"codi_emilio/resultats/DM1"
#

if (mostra) 
{nom_out<-here::here(dir_results,paste0("ResultatsEPI_mostra_",Sys.Date()))} else 
{nom_out<-here::here(dir_results,paste0("ResultatsEPI_poblacio_",Sys.Date()))}

rmarkdown::render(here::here("codi_emilio","2_preparacio_emilio.Rmd"),
                  params=list(mostra= mostra,subgrup=subgrup), 
                  output_file=nom_out)


# 2.3
rm(list = ls())
gc()
mostra= T
subgrup= "DM1" # "DM1"  # "Overall" "DM1" "DM2"

#
if (subgrup=="Overall") dir_results<-"codi_emilio/resultats/Overall"
if (subgrup=="DM2") dir_results<-"codi_emilio/resultats/DM2"
if (subgrup=="DM1") dir_results<-"codi_emilio/resultats/DM1"
#

if (mostra) 
{nom_out<-here::here(dir_results,paste0("ResultatsEPI_mostra_",Sys.Date()))} else 
{nom_out<-here::here(dir_results,paste0("ResultatsEPI_poblacio_",Sys.Date()))}

rmarkdown::render(here::here("codi_emilio","2_preparacio_emilio.Rmd"),
                  params=list(mostra= mostra,subgrup=subgrup),
                  output_file=nom_out)





