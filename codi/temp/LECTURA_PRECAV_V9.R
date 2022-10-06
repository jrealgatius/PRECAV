#####################  PROCES LECTURA DE DADES ----------------------------------
#
####  Aplicació de criteris d'inclusió i lectura de dades     

memory.size(max=130685)

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

# - AGREGADOR DE CAS = CARD_ISQ (Ojo de moment considero event = "CARD_ISQ")
AGR_CAS="CARD_ISQ"
# 
# fitxersortida
fitxersortida<-"BD_PRECAV.rds"

# Nombre de controls per cas
num_controls<-10

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
PACIENTS<-readRDS("ECV_CAT_entregable_pacients_20181128_152759.rds")

PROBLEMES<-readRDS("ECV_CAT_entregable_problemes_20181123_172533.rds")
CMBDH<-readRDS("ECV_CAT_entregable_cmbd_dx_20181123_172533.rds")
CMBDH.padris<-readRDS("ECV_CAT_entregable_cmbd_dx_padris_20181123_172533.rds")
CMBDH_PROC<-readRDS("ECV_CAT_entregable_cmbd_px_padris_20181123_172533.rds")

FX.FACTURATS<-readRDS("ECV_CAT_entregable_facturacions_20181123_172533.rds")

FX.PRESCRITS<-readRDS("ECV_CAT_entregable_prescripcions_20181123_172533.rds")

VARIABLES<-readRDS("ECV_CAT_entregable_variables_analitiques_20181123_172533.rds")
CLINIQUES<-readRDS("ECV_CAT_entregable_variables_cliniques_20181123_172533.rds")

TABAC<-readRDS("ECV_CAT_entregable_tabaquisme_20181123_172533.rds")

VISITES<-readRDS("ECV_CAT_entregable_visites_20181123_172533.rds")
DERIVACIONS<-readRDS("ECV_CAT_entregable_derivacions_20181123_172533.rds")

CATALEG<-readRDS("ECV_CAT_entregable_cataleg_20181123_172533.rds")

# library("xlsx")
# write.xlsx(CATALEG,file="cataleg.xlsx")

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
  filter (!(situacio=="D" & sortida<=20100101)) %>%  # Excloc: Difunts anterior a 2010
  data.table()

# 3. Identificar Casos i fusionar problemes de salut   ---------------------------

####  PROBLEMES DE SALUT PER IDENTIFICAR EVENTS CV POTENCIALS  

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
## Identificació de casos i data index 



# Agrego problemes amb límit data máxima (31/12/2016) O sigui dels diagnostics agafo la primera data de cada problema de salut

#### Fusiono agregador (+agr) 

dates_casos<-PROBLEMES_total %>% 
  agregar_problemes(bd.dindex="20161231",dt.agregadors=CATALEG,prefix="") %>%   # agrego problemes de salut
  select_("idp","dtevent"=AGR_CAS) %>%         ## Selecciono CARD_ISQ que serà la data index dels events 
  drop_na() %>%                     ## borro missing
  ungroup()

#     CAPTURO DATA EVENT D'INTERES      
### Data d'esdeveniment CV Potencial a partir de : dtevent

### Afegeixo a Cohort (PACIENTS) + Casos amb la data d'esdeveniment

PACIENTS<-PACIENTS %>% 
  dplyr::left_join(dates_casos, by="idp") %>% 
  as_tibble()

### Criteri d'exclusió 2 ANTECEDENT CV -------------
### Genero variable ANTECEDENT i EVENT (CAS) i selecciono la cohort de pacients sense ANTECEDENTS
####  CONSIDERO 

PACIENTS<-PACIENTS %>% 
  mutate (ANT.event=ifelse(dtevent<=20100101,1,0)) %>% 
  mutate (ANT.event=ifelse(is.na(dtevent), 0, ANT.event)) %>% 
  mutate (event=ifelse(dtevent>20100101,1,0)) %>% 
  mutate (event=ifelse(is.na(dtevent), 0, event)) %>% 
  filter(ANT.event==0) %>% ### SELECCIONO PACIENTS SENSE ANTECEDENTS CV
  select(-ANT.event)

# 4. Algoritme de SELECCIÓ DE CONTROLS           ----------------
##
##   0.	Assignar una data de sortida (data màxima a risc de tenir un esdeveniment)
##        	31/12/2016 / Data de mort-trasllat  / Data de primer EVENT )

# Event data sortida = data event
# Defunció data sortida = Data defuncio última de seguiment (12/2016)
# altrament --> 20161231

PACIENTS <- PACIENTS %>% 
  mutate (dtsortida=ifelse(situacio=="D" | situacio=="T" ,sortida,20161231)) %>%     ### Difunts/Trasladats ---> Data sortida
  mutate (dtsortida=ifelse(event==1,dtevent,dtsortida))            ### tots els Events data de sortida -> data event

###############       APARELLAMENT by riskSetMatch                         
###############               Preparo la base de dades per l'aparellamenta                    

#######  Hi funsiono data random en a pacients       
dt=PACIENTS


# dt <-dt %>% 
#   left_join(BD_PAC_DINDEX,by="idp") %>% select(-CARD_ISQ)

######          dtindex_case = Data event
dt<-dt %>% mutate(
  dtindex_case=ifelse(event==1, as.integer(lubridate::ymd(dtsortida)),NA)) 

####            Data de censura en controls (dtsortida)   dtindex_control
dt<-dt %>% mutate (dtindex_control=as.integer(lubridate::ymd(dtsortida)))

####  Genero any de naixament / grup cada 10 
dt<-dt %>% mutate (
  any_naix=lubridate::year(lubridate::ymd(dnaix)),
  any_naix_g10=Hmisc::cut2(any_naix,g=10),
  any_naix_g17=Hmisc::cut2(any_naix,seq(from = 1935, to = 1974, by = 2))
  ) 

#     Seleccio de Controls  ---------

################     FUNCIÓ riskSetMatch          
### Previament HE D'INSTALAR heaven 
# install.packages("devtools")
# install.packages("githubinstall")
# library(githubinstall)
# githubinstall("heaven")

library(heaven)

#### Llista d'aparellamenta

# llistaPS=c("sexe","any_naix","idup")

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

pp<-pp %>% as.data.table()


matchReport(pp, "idp",case="event",caseid="caseid")

# Número de controls per conjut a risk 
pp[,numControls:=.N,by=caseid]

#     Verificació de Matching -----------------------

# llistaPS=c("sexe","any_naix","idup")

llista.compare.Ys(dt=pp,llista.y=c("event"),llista.x=llistaPS,show.ratio=F,byrow=F)



# 5. Agregar en data index -------------------------------
pp<-pp %>% mutate (
  dataindex=ifelse(event==1,dtindex_case,dtindex_control),
  dataindex=as.Date(dataindex,origin = "1970-01-01"))

bdades_index<-pp %>% select(idp,dataindex)


#     Agregar visites -------------
visites_bd<-agregar_visites(dt=VISITES,bd.dindex = bdades_index,finestra.dies=c(-365,0))
visites_bd

#     Agregar Problemes ----------------
problemes_bd<-agregar_problemes(dt=PROBLEMES_total,bd.dindex =bdades_index,dt.agregadors=CATALEG,finestra.dies=c(-Inf,0))

problemes_bd

#     Agregar Analitiques ---------------
### Juntar totes les bdades de 
VARIABLES<-VARIABLES %>% select(idp,agr=cod,val,dat) 
CLINIQUES<-CLINIQUES %>% select(idp,agr=cod,val,dat) 

ANALITIQUES_total<-
  VARIABLES %>% 
  rbind(CLINIQUES) 

analitiques_bd<-agregar_analitiques(dt=ANALITIQUES_total,bd.dindex =bdades_index,finestra.dies=c(-365,0))


#     Agregar TABAC ---------------------------

TABAC<-TABAC %>% mutate(cod="tab")

tabac_bd<-agregar_analitiques(dt=TABAC,bd.dindex =bdades_index,finestra.dies=c(-Inf,0))


#     Agregar Farmacs Facturació ---------------------------

farmacs_dispensats<-agregar_facturacio(dt=FX.FACTURATS,prefix = "FD.",bd.dindex =bdades_index,finestra.dies=c(-365,0),dt.agregadors=CATALEG)


#     Agregar Farmacs Prescrits ----------------------

farmacs_prescrits<-agregar_prescripcions(dt=FX.PRESCRITS,bd.dindex=bdades_index,dt.agregadors=CATALEG,prefix="FP.")

  

#     Fusionar totes les bases de dades -----------------------
# BDINDEX + PACIENTS + PROBLEMES + VARIABLES + VISITES + TABAC + FARMACS

# bdades_index + pp + problemes_bd + analitiques_bd + visites_bd + tabac_bd +farmacs_dispensats + farmacs_prescrits


BDTOTAL<-bdades_index %>% 
  left_join(pp, by="idp") %>% 
  left_join(problemes_bd, by="idp") %>% 
  left_join(analitiques_bd, by="idp") %>% 
  left_join(tabac_bd, by="idp") %>% 
  left_join(farmacs_dispensats, by="idp") %>% 
  left_join(farmacs_prescrits, by="idp") %>% 
  left_join(visites_bd, by="idp") %>% 
  as.data.table()


saveRDS(BDTOTAL,fitxersortida)


