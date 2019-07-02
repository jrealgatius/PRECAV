
memory.size(max=160685)
#
#####################  Directori Font     ==============================  

rm(list=ls())
###
directori.arrel<-c("C:/Users/Jordi/Google Drive", 
                   "C:/Users/usuari/Google Drive",
                   "C:/Users/43728088M/Google Drive",
                   "C:/Users/jreal/Google Drive",
                   "D:/Google Drive",
                   "G:/Google Drive",
                   "E:/Google Drive")


library(dplyr)
directori.arrel[file.exists(directori.arrel)] %>% 
  file.path("Stat_codis/funcions_propies.R") %>% 
  source()

####    DIRECTORI DE TREBALL              
#### setwd en directori de treball 

"CIBERDEM/GEDAPS/PRECAV/DADESSIDIAP2" %>% 
  directori_treball(directori.arrel)


### LECTURA ---------------------------

# CATALEG<-readRDS("ECV_CAT_entregable_cataleg_20190517_101801.rds")
# library("xlsx")
LLEGIR.PACIENTS<-function(n=Nmostra) {
  readRDS("ECV_CAT_entregable_pacients_20190517_101801.rds") %>% as_tibble() %>% head(n)}

LLEGIR.PROBLEMES<-function(n=Nmostra) {
  readRDS("ECV_CAT_entregable_problemes_20181123_172533.rds")%>% as_tibble() %>% head(n)}

LLEGIR.CMBDH<-function(n=Nmostra) {
  readRDS("ECV_CAT_entregable_cmbd_dx_20181123_172533.rds")%>% as_tibble() %>% head(n)}

LLEGIR.padris<-function(n=Nmostra) {
  readRDS("ECV_CAT_entregable_cmbd_dx_padris_20181123_172533.rds")%>% as_tibble() %>% head(n)}

LLEGIR.PROC<-function(n=Nmostra) {
  readRDS("ECV_CAT_entregable_cmbd_px_padris_20181123_172533.rds")%>% as_tibble() %>% head(n)}

LLEGIR.TABAC<-function(n=Nmostra) {
  readRDS("ECV_CAT_entregable_tabaquisme_20181123_172533.rds")%>% as_tibble() %>% head(n) }

LLEGIR.DERIVACIONS<-function(n=Nmostra) {
  readRDS("ECV_CAT_entregable_derivacions_20181123_172533.rds")%>% as_tibble() %>% head(n) }

LLEGIR.FX.FACTURATS<-function(n=Nmostra) {
  readRDS("ECV_CAT_entregable_facturacions_20190517_101801.rds")%>% as_tibble() %>% head(n) }

LLEGIR.FX.PRESCRITS<-function(n=Nmostra) {
  readRDS("ECV_CAT_entregable_prescripcions_20190517_101801.rds")%>% as_tibble() %>% head(n) }

LLEGIR.VARIABLES<-function(n=Nmostra) {
  readRDS("ECV_CAT_entregable_variables_analitiques_20181123_172533.rds")%>% as_tibble() %>% head(n) }

LLEGIR.CLINIQUES<-function(n=Nmostra) {
  readRDS("ECV_CAT_entregable_variables_cliniques_20181123_172533.rds")%>% as_tibble() %>% head(n) }

LLEGIR.VISITES<-function(n=Nmostra) {
  readRDS("ECV_CAT_entregable_visites_20181123_172533.rds")%>% as_tibble() %>% head(n) }

##  Llegir 

Nmostra<-Inf

# Funció per seleccionar mostra random 
PACIENTS<-Inf %>% LLEGIR.PACIENTS()
pacients_mostra<-mostreig_ids(PACIENTS,"idp",n_mostra = 400000)
rm(PACIENTS)
saveRDS(pacients_mostra,file="./dades_test/pacients_mostra.rds")

# Salvar_en sudirectori
PROBLEMES<-Nmostra %>% LLEGIR.PROBLEMES()
PROBLEMES_mostra<-pacients_mostra %>% select(idp) %>% inner_join(PROBLEMES,by="idp")

CMBDH<-Nmostra %>% LLEGIR.CMBDH()
CMBDH_mostra<-pacients_mostra %>% select(idp) %>% inner_join(CMBDH,by="idp")

CMBDH.padris<-Nmostra %>% LLEGIR.padris()
CMBDH.padris_mostra<-pacients_mostra %>% select(idp) %>% inner_join(CMBDH.padris,by="idp")

CMBDH_PROC<-Nmostra %>% LLEGIR.PROC()
CMBDH_PROC_mostra<-pacients_mostra %>% select(idp) %>% inner_join(CMBDH_PROC,by="idp")


rm(CMBDH)
rm(CMBDH_PROC)
rm(CMBDH.padris)
gc()

saveRDS(PROBLEMES_mostra,file="./dades_test/PROBLEMES_mostra.rds")
saveRDS(CMBDH_mostra,file="./dades_test/CMBDH_mostra.rds")
saveRDS(CMBDH.padris_mostra,file="./dades_test/CMBDH.padris_mostra.rds")
saveRDS(CMBDH_PROC_mostra,file="./dades_test/CMBDH_PROC_mostra.rds")


# Variables ---------------
VARIABLES<-Nmostra %>% LLEGIR.VARIABLES() %>% select(idp,cod,val,dat) 
VARIABLES_mostra<-pacients_mostra %>% select(idp) %>% inner_join(VARIABLES,by="idp")
rm(VARIABLES)
gc()
saveRDS(VARIABLES_mostra,file="./dades_test/VARIABLES_mostra.rds")

rm(VARIABLES_mostra)

# Cliniques ---------------
CLINIQUES<-Nmostra %>% LLEGIR.CLINIQUES() 
CLINIQUES_mostra<-pacients_mostra %>% select(idp) %>% inner_join(CLINIQUES,by="idp")
rm(CLINIQUES)
gc()
saveRDS(CLINIQUES_mostra,file="./dades_test/CLINIQUES_mostra.rds")

# TAbac -------------
TABAC<-Nmostra %>% LLEGIR.TABAC () 
TABAC_mostra<-pacients_mostra %>% select(idp) %>% inner_join(TABAC,by="idp")
rm(TABAC)
gc()

saveRDS(TABAC_mostra,file="./dades_test/TABAC_mostra.rds")

# Farmacs facturats -------------
FX.FACTURATS<-Nmostra %>% LLEGIR.FX.FACTURATS()

FX.FACTURATS_mostra<-pacients_mostra %>% select(idp) %>% inner_join(FX.FACTURATS,by="idp")
rm(FX.FACTURATS)
gc()

saveRDS(FX.FACTURATS_mostra,file="./dades_test/FX.FACTURATS_mostra.rds")

# Farmacs prescrits -------------
FX.PRESCRITS<-Nmostra %>% LLEGIR.FX.PRESCRITS
FX.PRESCRITS_mostra<-pacients_mostra %>% select(idp) %>% inner_join(FX.PRESCRITS,by="idp")
rm(FX.PRESCRITS)
gc()
saveRDS(FX.PRESCRITS_mostra,file="./dades_test/FX.PRESCRITS_mostra.rds")


# VISITES ------------------
VISITES<-Nmostra %>% LLEGIR.VISITES()
VISITES_mostra<-pacients_mostra %>% select(idp) %>% inner_join(VISITES,by="idp")

rm(VISITES)
gc()

saveRDS(VISITES_mostra,file="./dades_test/VISITES_mostra.rds")

rm(VISITES_mostra)
