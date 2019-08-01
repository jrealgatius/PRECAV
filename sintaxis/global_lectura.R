#  1. Lectura de Fitxers   (Mostra / o poblacional) --------------------------

# ECV_CAT_entregable_pacients_20181128_152759.rds
# ECV_CAT_entregable_pacients_20190517_101801.rds

# ECV_CAT_entregable_problemes_20181123_172533.rds
# ECV_CAT_entregable_cmbd_dx_20181123_172533.rds
# ECV_CAT_entregable_cmbd_dx_padris_20181123_172533.rds
# ECV_CAT_entregable_cmbd_px_padris_20181123_172533.rds

# ECV_CAT_entregable_facturacions_20190517_101801.rds
# ECV_CAT_entregable_prescripcions_20190517_101801.rds

# ECV_CAT_entregable_variables_analitiques_20181123_172533.rds
# ECV_CAT_entregable_variables_cliniques_20181123_172533.rds
# ECV_CAT_entregable_tabaquisme_20181123_172533.rds

# ECV_CAT_entregable_visites_20181123_172533.rds
# ECV_CAT_entregable_derivacions_20181123_172533.rds

# ECV_CAT_entregable_cataleg_20190517_101801.rds

funcions_lectura_dades<-function(mostra) {

  # Si mostra --> Llegir fitxers mostra
if (mostra==T) {

  LLEGIR.PACIENTS<<-function(n=Nmostra) {
    readRDS("./dades/sidiap_test/pacients_mostra.rds") %>% as_tibble() %>% head(n)}
  
  LLEGIR.PROBLEMES<<-function(n=Nmostra) {
    readRDS("./dades/sidiap_test/PROBLEMES_mostra.rds")%>% as_tibble() %>% head(n)}
  
  LLEGIR.CMBDH<<-function(n=Nmostra) {
    readRDS("./dades/sidiap_test/CMBDH_mostra.rds") %>% as_tibble() %>% head(n)}
  
  LLEGIR.padris<<-function(n=Nmostra) {
    readRDS("./dades/sidiap_test/CMBDH_PROC_mostra.rds") %>% as_tibble() %>% head(n)}
  
  LLEGIR.PROC<<-function(n=Nmostra) {
    readRDS("./dades/sidiap_test/CMBDH.padris_mostra.rds") %>% as_tibble() %>% head(n)}
  
  LLEGIR.TABAC<<-function(n=Nmostra) {
    readRDS("./dades/sidiap_test/TABAC_mostra.rds") %>% as_tibble() %>% head(n) }
  
  LLEGIR.FX.FACTURATS<<-function(n=Nmostra) {
    readRDS("./dades/sidiap_test/FX.FACTURATS_mostra.rds")%>% as_tibble() %>% head(n) }
  
  LLEGIR.FX.PRESCRITS<<-function(n=Nmostra) {
    readRDS("./dades/sidiap_test/FX.PRESCRITS_mostra.rds")%>% as_tibble() %>% head(n) }
  
  LLEGIR.VARIABLES<<-function(n=Nmostra) {
    readRDS("./dades/sidiap_test/VARIABLES_mostra.rds")%>% as_tibble() %>% head(n) }
  
  LLEGIR.CLINIQUES<<-function(n=Nmostra) {
    readRDS("./dades/sidiap_test/CLINIQUES_mostra.rds")%>% as_tibble() %>% head(n) }
  
  LLEGIR.VISITES<<-function(n=Nmostra) {
    readRDS("./dades/sidiap_test/VISITES_mostra.rds")%>% as_tibble() %>% head(n) }
}

  # Llegir fitxers globals
  
if (mostra==F) {
  
  LLEGIR.PACIENTS<<-function(n=Nmostra) {
    readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_pacients_20190517_101801.rds")) %>% as_tibble() %>% head(n)}
  
  LLEGIR.PROBLEMES<<-function(n=Nmostra) {
    readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_problemes_20181123_172533.rds"))%>% as_tibble() %>% head(n)}
  
  LLEGIR.CMBDH<<-function(n=Nmostra) {
    readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_cmbd_dx_20181123_172533.rds"))%>% as_tibble() %>% head(n)}
  
  LLEGIR.padris<<-function(n=Nmostra) {
    readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_cmbd_dx_padris_20181123_172533.rds"))%>% as_tibble() %>% head(n)}
  
  LLEGIR.PROC<<-function(n=Nmostra) {
    readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_cmbd_px_padris_20181123_172533.rds"))%>% as_tibble() %>% head(n)}
  
  LLEGIR.TABAC<<-function(n=Nmostra) {
    readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_tabaquisme_20181123_172533.rds"))%>% as_tibble() %>% head(n) }
  
  LLEGIR.DERIVACIONS<<-function(n=Nmostra) {
    readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_derivacions_20181123_172533.rds"))%>% as_tibble() %>% head(n) }
  
  LLEGIR.FX.FACTURATS<<-function(n=Nmostra) {
    readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_facturacions_20190705_071704.rds"))%>% as_tibble() %>% head(n) }
  
  LLEGIR.FX.PRESCRITS<<-function(n=Nmostra) {
    readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_prescripcions_20190705_071704.rds"))%>% as_tibble() %>% head(n) }
  
  LLEGIR.VARIABLES<<-function(n=Nmostra) {
    readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_variables_analitiques_20181123_172533.rds"))%>% as_tibble() %>% head(n) }
  
  LLEGIR.CLINIQUES<<-function(n=Nmostra) {
    readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_variables_cliniques_20181123_172533.rds"))%>% as_tibble() %>% head(n) }
  
  LLEGIR.VISITES<<-function(n=Nmostra) {
    readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_visites_20181123_172533.rds"))%>% as_tibble() %>% head(n) }
  
  }
  

}