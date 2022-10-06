#########################     LECTURA DE DADES          #################################
#
####  Aplicació de criteris d'inclusió i lectura de dades     ###########################

#
#####################   SOURCE        #######################

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

###################   DIRECTORI DE TREBALL              ####
#### setwd en directori de treball 

"CIBERDEM/GEDAPS/PRECAV/DADESSIDIAP" %>% 
  directori_treball(directori.arrel)


####################################     LLISTA DE FITXERS        #########################

# ECV_CAT_entregable_pacients_20180118_114009.txt

# ECV_CAT_entregable_problemes_20180118_114009.txt
# ECV_CAT_entregable_cmbd_dx_20180118_114009.txt

# ECV_CAT_entregable_variables_analitiques_20180122_155228.txt
# ECV_CAT_entregable_variables_cliniques_20180122_150054.txt

# ECV_CAT_entregable_tabaquisme_20180118_114009.txt

# ECV_CAT_entregable_prescripcions_20180118_114009.txt
# ECV_CAT_entregable_facturacions_20180118_114009.txt

# CATALEG

#########################################################################################
#
### LECTURA ######

library(readr)

##### Llegir pacients     #######
#                 PACIENTS                #.
PACIENTS<-fread('ECV_CAT_entregable_pacients_20180118_114009.txt',sep="|",nrows = 50000)



#####   CRITERIS D'INCLUSIÓ   ####

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


####  Llegir PROBLEMES DE SALUT PER IDENTIFICAR EVENTS CV POTENCIALS  

PROBLEMES<-fread('ECV_CAT_entregable_problemes_20180118_114009.txt',sep="|",nrows = 2000000)

PROBLEMES %>% group_by(agr) %>% summarise(n())

CMBD<-fread("ECV_CAT_entregable_cmbd_dx_20180118_114009.txt",sep="|",nrows=2000000)


#### Fusionar PROBLEMES + CMBDH ### Seleccionar 

diagnostics<-PROBLEMES %>%
  bind_rows(CMBD) %>% 
  select(idp,cod,dat) %>% 
  as_tibble()


####  CATALEG per AGREGAR R       #####
## De momnent EL D'EPI DM1

CATALEG<-fread("epiDM1_entregable_cataleg_20180621_103810.txt",sep="|")


# Agrego problemes amb límit data máxima (31/12/2016) O sigui dels diagnostics agafo la primera data de cada problema de salut

dates_casos<-diagnostics %>% 
  agregar_problemes(dataindex="20161231",dt.agregadors=CATALEG) %>%   ## agrego problemes de salut
  select(idp, CARD_ISQ) %>%         ## Selecciono CARD_ISQ que serà la data index 
  drop_na() %>%                     ## borro missing
  ungroup()

# i fusiono amb pacients 

####################     CAPTURO DATA EVENT D'INTERES      ################
### Data d'esdeveniment CV Potencial a partir de : CARD_ISQ

### Afegeixo a Cohort (PACIENTS) + Casos amb la data d'esdeveniment

PACIENTS<-PACIENTS %>% 
  dplyr::left_join(dates_casos, by="idp") %>% 
  as_tibble()


### Genero variables antecedents i events CV  i selecciono la cohort de pacients

####  CONSIDERO event= CARD_ISQ

PACIENTS<-PACIENTS %>% 
  mutate (ANT.CI=ifelse(CARD_ISQ<=20100101,1,0)) %>% 
  mutate (ANT.CI=ifelse(is.na(CARD_ISQ), 0, ANT.CI)) %>% 
  mutate (event=ifelse(CARD_ISQ>20100101,1,0)) %>% 
  mutate (event=ifelse(is.na(CARD_ISQ), 0, event)) %>% 
  filter(ANT.CI==0)     ### SELECCIONO PACIENTS SENSE ANTECEDENTS CV


## Algoritme de selecció de controls
## Algoritme de selecció de controls
## Algoritme de selecció de controls

##
### 0.	Assignar una data de sortida (data màxima a risc de tenir un esdeveniment)
#   i.	31/12/2016 / Data de mort  / Data de primer esdeveniment CV)

# EV.CI --> (Event) data sortida = data event
# Defunció data sortida = Data defuncio última de seguiment (12/2016)
# altrament --> 20161231

PACIENTS <- PACIENTS %>% 
  mutate (dtsortida=ifelse(situacio=="D",sortida,20161231)) %>%   ### Difunts ---> Data sortida
  mutate (dtsortida=ifelse(event==1,CARD_ISQ,dtsortida))             ### Events data de sortida (CARD_ISQ)


### Assignar potencials dates index

# a) Random (amb un màxim de data de sortida)
# b) Semi Random

# La random entre : 20100101 --> dtsortida 


#                      a) DATA Random (amb un màxim de data A data sortida)

######################    FUNCIÓ RANDOM en periode (2010-2016)      ########################
#

##################################    B) SEMI.RANDOM         #################

#                      b) SEMI.RANDOM (amb un màxim de data A data sortida)

# b.	Semi-Random : Una data entre tots Colesterol total (prèvies a data sortida) 
# Si no hi ha cap Colesterol  alguna V clínica període (Random) 


#######################       dtindex.semirandom    ####

### llegeixo variablesclinm

# ECV_CAT_entregable_variables_analitiques_20180122_155228.txt
# ECV_CAT_entregable_variables_cliniques_20180122_150054.txt

#                 V.CLINIQUES                       #.
VARIABLES<-fread('ECV_CAT_entregable_variables_cliniques_20180122_150054.txt',sep="|",nrows = 1000000)


### Per cada pacient selecciono una dat random de entre tots els COLESTEROLS  (2010-2016)
UN.COLESTEROL<-VARIABLES %>%               
  filter(cod=="EK201") %>%                # selecciono colesterols (Validar que es EK201)
  dplyr::left_join(PACIENTS,by="idp") %>% # Junto pacients 
  select(idp,cod,dat,dtsortida) %>%       # Selecciono camps necessaris
  filter(!is.na(dtsortida)) %>%               # Filtro només pacients (amb dtsortida)
  filter (dat>20100101 & dat<=20161231) %>% # filtro només entre dates 
  group_by(idp) %>%                       # Agafo un colesterol per cada idp
  sample_n(size = 1) %>% 
  ungroup %>% 
  select(idp, dat) %>% 
  rename(dat_col=dat) 
  

### Per cada pacient selecciono una dat random entre totes les VARIABLES 
UNA.VARIABLE<-VARIABLES %>%               # totes les variables  
  dplyr::left_join(PACIENTS,by="idp") %>% # Junto pacients 
  select(idp,dat,dtsortida) %>%       # Selecciono camps necessaris
  filter(!is.na(dtsortida)) %>%               # Filtro només pacients amb dnaix
  filter (dat>20100101 & dat<=20161231) %>% # filtro només entre dates 
  group_by(idp) %>%                       # Agafo unA fila per cada idp
  sample_n(size = 1) %>% 
  ungroup() %>% 
  select(idp, dat) %>% 
  rename(dat_var=dat)

  
### fUSIONO AMB PACIENTS DATES SEMIRANDOM + CONTROLS POTENCIALS

PACIENTS<-PACIENTS %>% 
  left_join(UN.COLESTEROL,by="idp") %>%     # Fusionar data colesterol
  left_join(UNA.VARIABLE,by="idp") %>%      # Fusionar data variable 
  mutate (
    dtindex.semirandom=ifelse(is.na(dat_col),dat_var,dat_col)) %>%   ## Agafar la que existeixi
  select(-dat_col,-dat_var,-up_sidiap_h,-sortida,-dtsortida)         # netejo  variables que no necessito
  
#



####################        FUNCIÓ QUE GENERA UNA DATA INDEX SEGONS UNA DETERMINACIÓ 

dt_index_data_semirandom<-function(dt=PACIENTS,dt.variables=VARIABLES,codi="EK201"){
  
  # dt=PACIENTS
  # dt.variables=VARIABLES
  # codi="EK201"
  
  # b) SEMI.RANDOM (amb un màxim de data a data sortida)
  
  # Una data entre tots Colesterol total (prèvies a data sortida) 
  # Si no hi ha cap Colesterol  alguna V clínica període (Random) 
  set.seed(123)
  ### Per cada pacient selecciono una dat random de entre tots els COLESTEROLS  (2010-2016)
  UN.COLESTEROL<-dt.variables %>%               
    filter(cod==codi) %>%                       # selecciono colesterols (Validar que es EK201)
    dplyr::left_join(dt,by="idp") %>%           # Junto pacients 
    select(idp,cod,dat,dtsortida) %>%           # Selecciono camps necessaris
    filter(!is.na(dtsortida)) %>%               # Filtro només pacients (amb dtsortida)
    filter (dat>=20100101 & dat<=dtsortida) %>%  # filtro Dates dins periode de seguiment 
    group_by(idp) %>%                           # Agafo un colesterol per cada idp
    sample_n(size = 1) %>%                      # Random
    ungroup %>% 
    select(idp, dat) %>% 
    rename(dat_col=dat) 
  
  
  ### Per cada pacient selecciono una dat random entre totes les VARIABLES 
  UNA.VARIABLE<-dt.variables %>%                # totes les variables  
    dplyr::left_join(dt,by="idp") %>%           # Junto pacients 
    select(idp,dat,dtsortida) %>%               # Selecciono camps necessaris
    filter(!is.na(dtsortida)) %>%               # Filtro només pacients amb dtsortida 
    filter (dat>=20100101 & dat<=dtsortida) %>% # Dates possibles dins el seguiment
    group_by(idp) %>%                           # Agafo unA fila per cada idp
    sample_n(size = 1) %>%                      # RAndom
    ungroup() %>% 
    select(idp, dat) %>% 
    rename(dat_var=dat)
  
  ### Fusió d'ambdos fitxers i selecciono una d'elles preferentment colesterol

  DADES_DT_INDEX<-UNA.VARIABLE %>% 
    left_join(UN.COLESTEROL,by="idp") %>% 
    mutate(dtindex.semirandom=ifelse(is.na(dat_col),dat_var,dat_col)) %>% 
    select(idp,dtindex.semirandom)
    

}




















###################     APLICO MATCHING           ###################

llistaPS<-c("sexe","edat","dtindex") # covaribles

dades_matching<-matching_case_control(dt=PACIENTS,variables.ps=llistaPS) 

## Comprovo aparellament 

llista.compare.Ys(dt=dades_matching,llista.y=c("event"),llista.x=llistaPS,show.ratio=F,byrow=T)



#########################################         FI MATCHING                                     #########################



