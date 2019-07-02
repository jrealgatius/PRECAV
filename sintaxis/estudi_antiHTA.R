### Estudi d'anti HTA ######


# 1. Seleccionar població en una data (Data d'esdeveniment)
# 2. Agregar quins son AntiHTA / No 
# 3. Agregar quins porten Farmac AntiHTA per grup 
# 4. Descriptiva



#####################  PROCES LECTURA DE DADES ----------------------------------
#
####  Aplicació de criteris d'inclusió i lectura de dades     


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

# 0. Inicialització de parametres  -----------------------------

# N test mostra a seleccionar  (Nmostra=Inf)

# Nmostra=Inf  # Seria tota la mostra
Nmostra=100000

# - AGREGADOR DE CAS = EV_CARD
AGR_CAS<-"ECV_TER"
# 

"dades/preparades" %>% here::here("BD_PRECAV_test3.rds")

# fitxersortida
fitxersortida<-"dades/preparades" %>% here::here("BD_PRECAV_test3.rds")

# fitxer conductor cataleg 
fitxer_conductor_cataleg<-"dades" %>% here::here("cataleg_4.6.19_arreglat.xls")

# fitxer conductor variables
fitxer_conductor_variables<-"dades" %>% here::here("variables_precav.xls")

# Nombre de controls per cas
num_controls<-4

# Llista d'aparellamenta
llistaPS=c("sexe","any_naix","idup")

# 1. Lectura de Fitxers  --------------------------

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

# LECTURA

# CATALEG<-readRDS("ECV_CAT_entregable_cataleg_20190517_101801.rds")
# library("xlsx")
# write.xlsx(CATALEG,file="cataleg.xlsx")


CATALEG<-readxl::read_excel(fitxer_conductor_cataleg,col_types = "text")


LLEGIR.PACIENTS<-function(n=Nmostra) {
  readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_pacients_20190517_101801.rds")) %>% as_tibble() %>% head(n)}

LLEGIR.PROBLEMES<-function(n=Nmostra) {
  readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_problemes_20181123_172533.rds"))%>% as_tibble() %>% head(n)}

LLEGIR.CMBDH<-function(n=Nmostra) {
  readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_cmbd_dx_20181123_172533.rds"))%>% as_tibble() %>% head(n)}

LLEGIR.padris<-function(n=Nmostra) {
  readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_cmbd_dx_padris_20181123_172533.rds"))%>% as_tibble() %>% head(n)}

LLEGIR.PROC<-function(n=Nmostra) {
  readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_cmbd_px_padris_20181123_172533.rds"))%>% as_tibble() %>% head(n)}

LLEGIR.TABAC<-function(n=Nmostra) {
  readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_tabaquisme_20181123_172533.rds"))%>% as_tibble() %>% head(n) }

LLEGIR.DERIVACIONS<-function(n=Nmostra) {
  readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_derivacions_20181123_172533.rds"))%>% as_tibble() %>% head(n) }

LLEGIR.FX.FACTURATS<-function(n=Nmostra) {
  readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_facturacions_20190517_101801.rds"))%>% as_tibble() %>% head(n) }

LLEGIR.FX.PRESCRITS<-function(n=Nmostra) {
  readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_prescripcions_20190517_101801.rds"))%>% as_tibble() %>% head(n) }

LLEGIR.VARIABLES<-function(n=Nmostra) {
  readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_variables_analitiques_20181123_172533.rds"))%>% as_tibble() %>% head(n) }

LLEGIR.CLINIQUES<-function(n=Nmostra) {
  readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_variables_cliniques_20181123_172533.rds"))%>% as_tibble() %>% head(n) }

LLEGIR.VISITES<-function(n=Nmostra) {
  readRDS("dades/sidiap" %>% here::here("ECV_CAT_entregable_visites_20181123_172533.rds"))%>% as_tibble() %>% head(n) }

##  Llegir 

PACIENTS<-Inf %>% LLEGIR.PACIENTS()
PROBLEMES<-Nmostra %>% LLEGIR.PROBLEMES()
CMBDH<-Nmostra %>% LLEGIR.CMBDH()
CMBDH.padris<-Nmostra %>% LLEGIR.padris()
CMBDH_PROC<-Nmostra %>% LLEGIR.PROC()

# 2. Aplicar criteris d'Inclusió 1    ----------------------------------------

#-	Antiguitat a (Data entrada a SIDIAP<01/2007)
#-	Amb com a mínim una determinació entre (2007-2010).* (data d’entrada a la cohort)
#-	Edat superior >=29 anys  i inferior a 75 anys a 01/2010 a data d’entrada a la cohort (07-10)  (per tant el casos tindran com a mínim 35 durant a 01/2010 y 75 anys inclòs)
#   Any de naixament <1981 and >1935
#-  Actius a 2010
####  Excloc: Difunts a 2010 

PACIENTS<-PACIENTS %>% 
  filter (dnaix>19350101 & dnaix<19811231) %>%      # Nascuts entre el 35 i l'any 81
  filter (entrada<20070101) %>%                     # Antiguitat SIDIAP anterior a 2007
  filter (!(situacio=="D" & sortida<=20100101))     # Excloc: Difunts anterior a 2010


# 3. Identificar Casos i fusionar problemes de salut   ---------------------------

# 3.1. Fusionar problemes de salut (IDENTIFICAR EVENTS CV POTENCIALS)

PROBLEMES<-PROBLEMES %>% select(idp,cod,dat) 
CMBDH<-CMBDH %>% select(idp,cod,dat) 
CMBDH.padris<-CMBDH.padris %>% select(idp,cod,dat) 
CMBDH_PROC<-CMBDH_PROC %>% select(idp,cod,dat) 

### Juntar totes les bdades de problemes
PROBLEMES_total<-
  PROBLEMES %>% 
  rbind(CMBDH) %>% 
  rbind(CMBDH.padris) %>% 
  rbind(CMBDH_PROC) 

rm(PROBLEMES)
rm(CMBDH)
rm(CMBDH_PROC)
rm(CMBDH.padris)

#    IDENTIFICACIÓ DE CASOS          
# Identificació de casos i data index  --------

# Agrego problemes amb límit data máxima (31/12/2016) O sigui dels diagnostics agafo la primera data de cada problema de salut

#### Fusiono agregador (+agr) 
CASOS_HTA<-PROBLEMES_total %>% 
  agregar_problemes_agr(bd.dindex="20161231",agregador ="HTA_O",dt.agregadors=CATALEG,finestra.dies=c(-Inf,0),prefix="",camp_agregador="agr")  # agrego problemes de salut
  
### Afegeixo a Cohort (PACIENTS) + Casos amb la data d'esdeveniment

PACIENTS<-PACIENTS %>% 
  dplyr::left_join(CASOS_HTA, by="idp") %>% 
  as_tibble()


# Agregar Farmacs Facturació ---------------------------
FX.FACTURATS<-Nmostra %>% LLEGIR.FX.FACTURATS()
farmacs_dispensats<-agregar_facturacio(dt=FX.FACTURATS,prefix = "FD.",bd.dindex ="20161231",finestra.dies=c(-365,0),dt.agregadors=CATALEG,camp_agregador="agr")

# Agregar Farmacs Prescrits  ---------------------------
FX.PRESCRITS<-Nmostra %>% LLEGIR.FX.PRESCRITS()
farmacs_prescrits<-agregar_prescripcions(dt=FX.PRESCRITS,bd.dindex="20161231",dt.agregadors=CATALEG,prefix="FP.",finestra.dies = c(-45,+45),camp_agregador="agr")


# Ara fusionar pacients amb faramcs ------------------


# Despres fer una descriptiva --------------------





