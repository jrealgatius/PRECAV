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
Nmostra=Inf

# - AGREGADOR DE CAS = EV_CARD
AGR_CAS<-"ECV_TER"
# 
# fitxersortida
fitxersortida<-"BD_PRECAV.rds"

# fitxer conductor cataleg 
fitxer_conductor_cataleg<-"cataleg_definitiu 22.1.19.xls"

# fitxer conductor variables
fitxer_conductor_variables<-"variables.xls"

# Nombre de controls per cas
num_controls<-4

# Llista d'aparellamenta
llistaPS=c("sexe","any_naix","idup")


# sample_test 
# Nsample_test<-100000

# 1. Lectura de Fitxers  --------------------------

# ECV_CAT_entregable_pacients_20181128_152759.rds

# ECV_CAT_entregable_problemes_20181123_172533.rds
# ECV_CAT_entregable_cmbd_dx_20181123_172533.rds
# ECV_CAT_entregable_cmbd_dx_padris_20181123_172533.rds
# ECV_CAT_entregable_cmbd_px_padris_20181123_172533.rds

# ECV_CAT_entregable_facturacions_20181123_172533.rds
# ECV_CAT_entregable_prescripcions_20181123_172533

# ECV_CAT_entregable_variables_analitiques_20181123_172533.rds
# ECV_CAT_entregable_variables_cliniques_20181123_172533.rds
# ECV_CAT_entregable_tabaquisme_20181123_172533.rds

# ECV_CAT_entregable_visites_20181123_172533.rds
# ECV_CAT_entregable_derivacions_20181123_172533.rds

# ECV_CAT_entregable_cataleg_20181123_172533.rds

### LECTURA

# CATALEG<-readRDS("ECV_CAT_entregable_cataleg_20181123_172533.rds")
# library("xlsx")
# write.xlsx(CATALEG,file="cataleg.xlsx")
CATALEG<-readxl::read_excel(fitxer_conductor_cataleg)

LLEGIR.PACIENTS<-function(n=Nmostra) {
  readRDS("ECV_CAT_entregable_pacients_20181128_152759.rds") %>% as_tibble() %>% head(n)}

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
  readRDS("ECV_CAT_entregable_facturacions_20181123_172533.rds")%>% as_tibble() %>% head(n) }

LLEGIR.FX.PRESCRITS<-function(n=Nmostra) {
  readRDS("ECV_CAT_entregable_prescripcions_20181123_172533.rds")%>% as_tibble() %>% head(n) }

LLEGIR.VARIABLES<-function(n=Nmostra) {
  readRDS("ECV_CAT_entregable_variables_analitiques_20181123_172533.rds")%>% as_tibble() %>% head(n) }

LLEGIR.CLINIQUES<-function(n=Nmostra) {
  readRDS("ECV_CAT_entregable_variables_cliniques_20181123_172533.rds")%>% as_tibble() %>% head(n) }

LLEGIR.VISITES<-function(n=Nmostra) {
  readRDS("ECV_CAT_entregable_visites_20181123_172533.rds")%>% as_tibble() %>% head(n) }

##  Llegir 

PACIENTS<-Nmostra %>% LLEGIR.PACIENTS()
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


############################     IDENTIFICACIÓ DE CASOS          
## Identificació de casos i data index  --------

# Agrego problemes amb límit data máxima (31/12/2016) O sigui dels diagnostics agafo la primera data de cada problema de salut

#### Fusiono agregador (+agr) 
dates_casos<-PROBLEMES_total %>% 
  agregar_problemes_agr(bd.dindex="20161231",agregador =AGR_CAS,dt.agregadors=CATALEG,finestra.dies=c(-Inf,0),prefix="",camp_agregador="AGR_TER") %>%  # agrego problemes de salut
  select_("idp","dtevent"=AGR_CAS,"codindex"="cod")


#
#     CAPTURO DATA EVENT D'INTERES      
### Data d'esdeveniment CV Potencial a partir de : dtevent

### Afegeixo a Cohort (PACIENTS) + Casos amb la data d'esdeveniment

PACIENTS<-PACIENTS %>% 
  dplyr::left_join(dates_casos, by="idp") %>% 
  as_tibble()


# 4. Criteri d'exclusió 2 antecedent CV -------------
### Genero variable ANTECEDENT i EVENT (CAS) i selecciono la cohort de pacients sense ANTECEDENTS
####  CONSIDERO 

PACIENTS<-PACIENTS %>% 
  mutate (ANT.event=ifelse(dtevent<=20100101,1,0)) %>% 
  mutate (ANT.event=ifelse(is.na(dtevent), 0, ANT.event)) %>% 
  mutate (event=ifelse(dtevent>20100101,1,0)) %>% 
  mutate (event=ifelse(is.na(dtevent), 0, event)) %>% 
  filter(ANT.event==0) %>% ### SELECCIONO PACIENTS SENSE ANTECEDENTS CV
  select(-ANT.event)


# 5. Algoritme de seleccio de controls           ----------------
##
#  5.1.	Assignar una data de sortida (data màxima a risc de tenir un esdeveniment)  -------
##        	31/12/2016 / Data de mort-trasllat  / Data de primer EVENT )

# Event data sortida = data event
# Defunció data sortida = Data defuncio última de seguiment (12/2016)
# altrament --> 20161231

PACIENTS <- PACIENTS %>% 
  mutate (dtsortida=ifelse(situacio=="D" | situacio=="T" ,sortida,20161231)) %>%     ### Difunts/Trasladats --> Data sortida
  mutate (dtsortida=ifelse(event==1,dtevent,dtsortida))            ### tots els Events data de sortida -> data event

##                        
#  5.2. Preparar base de dades per l'aparellament ------------                    

#######  Hi funsiono data random en a pacients       
dt=PACIENTS


#     5.2.1. Generar data de sortida (Data event / Data de censura)     -----------------

## dtindex_case 

dt<-dt %>% mutate(
  dtindex_case=ifelse(event==1, as.integer(lubridate::ymd(dtsortida)),NA)) 

## dtindex_control
dt<-dt %>% mutate (dtindex_control=as.integer(lubridate::ymd(dtsortida)))

## 4.2.2. Generar any de naixament i grups cada 10 
dt<-dt %>% mutate (
  any_naix=lubridate::year(lubridate::ymd(dnaix)),
  any_naix_g10=Hmisc::cut2(any_naix,g=10),
  any_naix_g17=Hmisc::cut2(any_naix,seq(from = 1935, to = 1974, by = 2))
  ) 


#     5.2.3. Generar conjunts a Risc by riskSetMatch  (Selecció de controls) ------------

#  5.3. Seleccio de Controls  ---------

################     FUNCIÓ riskSetMatch          
### Previament HE D'INSTALAR heaven 
# install.packages("devtools")
# install.packages("githubinstall")
# library(githubinstall)
# githubinstall("heaven")

library(heaven)

#### Llista d'aparellamenta

# llistaPS=c("sexe","any_naix","idup")

set.seed(123)


pp<-riskSetMatch(ptid="idp"                                # Unique patient identifier
                 ,event="event"                # 0=Control, 1=case
                 ,terms=llistaPS   # terms c("n1","n2",...) - list of vairables to match by
                 ,dat=dt                       # dataset with all variables
                 ,Ncontrols=num_controls       # number of controls to provide
                 ,oldevent="oldevent"          # To distinguish cases used as controls
                 ,caseid="caseid"              # variable to group cases and controls (case-ptid)
                 ,reuseCases=T                 # T og F or NULL - can a case be a control prior to being a case?
                 ,reuseControls=F              # T or F or NULL - can controls be reused?
                 ,caseIndex="dtindex_case"       # Integer or date, date where controls must be prior
                 ,controlIndex="dtindex_control" # controlIndex - Index date for controls
                 ,NoIndex=FALSE                # If T ignore index
                 ,cores=1                      # Number of cores to use, default 1
                 ,dateterms=NULL               # character list of date variables
)



matchReport(pp, "idp",case="event",caseid="caseid")

# Número de controls per conjut a risk 
pp[,numControls:=.N,by=caseid]

#  5.4. Verificació de Matching -----------------------

# llistaPS=c("sexe","any_naix","idup")

llista.compare.Ys(dt=pp,llista.y=c("event"),llista.x=llistaPS,show.ratio=F,byrow=F)


# 6. Agregar en data index -------------------------------
pp<-pp %>% mutate (
  dataindex=ifelse(event==1,dtindex_case,dtindex_control),
  dataindex=as.Date(dataindex,origin = "1970-01-01"))


bdades_index<-pp %>% select(idp,dataindex) %>% as_tibble()

#     6.1. Agregar visites -------------
VISITES=Nmostra %>% LLEGIR.VISITES
visites_bd<-agregar_visites(dt=VISITES,bd.dindex = bdades_index,finestra.dies=c(-365,0))
rm(VISITES)

#     6.2. Agregar Problemes(Agrupadors antics) ----------------
problemes_bd<-agregar_problemes(dt=PROBLEMES_total,bd.dindex =bdades_index,dt.agregadors=CATALEG,finestra.dies=c(-Inf,0))

#     6.2.1 Agregar Problemes (ECV Manel) ----------------
problemesDG2_bd<-agregar_problemes(dt=PROBLEMES_total,bd.dindex =bdades_index,dt.agregadors=CATALEG,finestra.dies=c(-Inf,0),camp_agregador="AGR2",prefix = "EV.")

#     6.2.2 Agregar Problemes (Events Principals) ----------------
problemesEVPR_bd<-agregar_problemes(dt=PROBLEMES_total,bd.dindex =bdades_index,dt.agregadors=CATALEG,finestra.dies=c(-Inf,0),camp_agregador="AGR_PR",prefix = "EV.")

#     6.2.3. Agregar Problemes (Events Ampliats) --------------------
problemesEVAM_bd<-agregar_problemes(dt=PROBLEMES_total,bd.dindex =bdades_index,dt.agregadors=CATALEG,finestra.dies=c(-Inf,0),camp_agregador="AGR_AMP",prefix = "EV.")

#     6.2.4. Agregar Problemes (Events Secundaris) -----------------
problemesEVSC_bd<-agregar_problemes(dt=PROBLEMES_total,bd.dindex =bdades_index,dt.agregadors=CATALEG,finestra.dies=c(-Inf,0),camp_agregador="AGR_SEC",prefix = "EV.")

rm(PROBLEMES_total)

#     6.3. Agregar Variables ---------------

VARIABLES<-Nmostra %>% LLEGIR.VARIABLES %>% select(idp,cod,val,dat) 
variables_bd<-agregar_analitiques(dt=VARIABLES,bd.dindex =bdades_index,finestra.dies=c(-365,0))
rm(VARIABLES)

#     6.4. Agregar Cliniques ---------------
CLINIQUES<-Nmostra %>% LLEGIR.CLINIQUES %>% select(idp,cod=agr,val,dat) 
cliniques_bd<-agregar_analitiques(dt=CLINIQUES,bd.dindex =bdades_index,finestra.dies=c(-365,0))

#     6.5. Agregar TABAC ---------------------------
TABAC<-Nmostra %>% LLEGIR.TABAC %>% mutate(cod="tab")
tabac_bd<-agregar_analitiques(dt=TABAC,bd.dindex =bdades_index,finestra.dies=c(-Inf,0))
rm(TABAC)

#     6.6. Agregar Farmacs Facturació ---------------------------
FX.FACTURATS<-Nmostra %>% LLEGIR.FX.FACTURATS
farmacs_dispensats<-agregar_facturacio(dt=FX.FACTURATS,prefix = "FD.",bd.dindex =bdades_index,finestra.dies=c(-365,0),dt.agregadors=CATALEG)
rm(FX.FACTURATS)

#     6.7. Agregar Farmacs Prescrits ----------------------
FX.PRESCRITS<-Nmostra %>% LLEGIR.FX.PRESCRITS
farmacs_prescrits<-agregar_prescripcions(dt=FX.PRESCRITS,bd.dindex=bdades_index,dt.agregadors=CATALEG,prefix="FP.")
rm(FX.PRESCRITS)

# 7. Fusionar totes les taules -----------------------
# BDINDEX + PACIENTS + PROBLEMES + VARIABLES + VISITES + TABAC + FARMACS

# bdades_index + pp + problemes_bd + analitiques_bd + visites_bd + tabac_bd +farmacs_dispensats + farmacs_prescrits


pp<-as_tibble(pp)

BDTOTAL<-pp %>% 
  left_join(problemes_bd, by="idp") %>% 
  left_join(problemesDG2_bd, by="idp") %>% 
  left_join(problemesEVPR_bd, by="idp") %>% 
  left_join(problemesEVAM_bd, by="idp") %>% 
  left_join(problemesEVSC_bd, by="idp") %>% 
  left_join(variables_bd, by="idp") %>% 
  left_join(cliniques_bd, by="idp") %>% 
  left_join(tabac_bd, by="idp") %>% 
  left_join(farmacs_dispensats, by="idp") %>% 
  left_join(farmacs_prescrits, by="idp") %>% 
  left_join(visites_bd, by="idp") 


saveRDS(BDTOTAL,fitxersortida)

conductor_variables<-names(BDTOTAL) %>% as_tibble()

write.csv2(conductor_variables,"variables_precav.txt")


