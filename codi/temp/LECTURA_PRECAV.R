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
#   a.	Última data possible de seguiment a risk de ser cas:
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
##    Genero Data index potencial (Random) dins periode d'estudi 2010-2016 
##    Selecciono pacients que estan dins del periode de seguiment

# set.seed(123)
# start_time <- Sys.time()
# BD_PAC_DINDEX<-PACIENTS %>% 
#   select(idp, dtsortida) %>%
#   dplyr::rowwise() %>% 
#   dplyr::mutate(
#     dtindex.random=data.random(20100101,20161231), ##### Genero la data random
#     control=ifelse(dtindex.random<=lubridate::ymd(dtsortida),1,0)) ######  Marca com a Control si està dins de periode de seguiment
# end_time <- Sys.time()
# end_time - start_time


BD_PAC_DINDEX<-dt_index_data_random(PACIENTS)


# Time difference with 73058 registers --> 5.84   minuts (Portatil)
# Time difference with 73058 registers --> 5.62   minuts (PC USR)
# Time difference with 73058 registers --> 5.02   minuts (PC USR)
# Time difference with 36787 registers --> 2.31 minuts (IGTP)
# Time difference with 36787 registers --> 5.63 minuts (Casa PC)
# Time difference with 36787 registers --> 3.92 minuts (Casa PC)
# Time difference with 36787 registers --> 3.20 minuts (Casa Portatil)
# Time difference with 36787 registers --> 3.9    minuts (PC USR)

# start_time <- Sys.time()
# BD_PAC_DINDEX[1:2000,] %>% 
#   # select(idp, dtsortida) %>% 
#   mutate(dtindex.random = unlist(pmap(list(dataini=20120101, datafi=dtsortida), data.random))) 
# end_time <- Sys.time()
# end_time - start_time

# Time difference of 7.58 secs amb 2000 registres (Igtp)
# Time difference of 7.7 secs amb 2000 registres (USR)
# Time difference of 8.683446 secs amb 2000 registres (USR)
# Time difference of 9.683446 secs amb 2000 registres (Casa)
# Time difference of 9.84 secs amb 2000 registres (Portatil)

########

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
  
PACIENTS <-PACIENTS %>% 
  left_join(BD_PAC_DINDEX,by="idp")                       # Hi fusiono data random generada + POTENCIAL CONTROL

####  transforma data a caracter 
library(lubridate)

PACIENTS<-PACIENTS %>% 
  mutate (dtindex.txt=data.to.string(dtindex.random))

dt<-PACIENTS

# Seleccionar controls i mutar dataindex en data index random 
dtcontrols<-dt %>% filter(control==1) %>% mutate(dtindex=dtindex.random, event=0)

# Fusionar events + controls 
dt.total<-dtevents %>% rbind(dtcontrols)


# Agrego en dtindex 
dt.total<-dt.total %>% 
  mutate (edat=as.numeric((dtindex-ymd(dnaix))/365.25)) %>%                # Calculo edat en dataindex
  mutate (dtindex=as.numeric(dtindex))                                     # Calculo data index com a numerica
# MatchiT 

# preparar dades per matching (idp + Llista matching)
dadesmatching<-dt.total %>% select(idp,sexe,edat,dtindex,event)

#
llistaPS<-c("sexe","edat","dtindex") # covaribles
formulaPS<-as.formula(paste("event", paste(llistaPS, collapse=" + "), sep=" ~ "))

set.seed(125)





#########################################         FI MATCHING                                     #########################























#### s'ha d'entrar una dades, dataindex, columna event, variables.ps 
### Retorna submostra de dades matchejades

llistaPS<-c("sexe","edat","dtindex") # covaribles

### primer agregació en data index


matching_case_control<-function(dt=dades,variables.ps=llistaPS) {
  
  dt=PACIENTS
  variables.ps=llistaPS

  
# 1 Generar data index en potencials controls (inclou events futurs)
  
  BD_PAC_DINDEX<-dt_index_data_random(dt)
  
  BD_PAC_DINDEX <- BD_PAC_DINDEX %>% mutate(control=1)
  
  
# 2 Fusionar events i controls 
  
  dt <-dt %>% 
    left_join(BD_PAC_DINDEX,by="idp")              # Hi fusiono data random generada + indicadora de control
  
  # Selecciono events i mutar dataindex (event=1) en dtindex 
  
  dtevents<-dt %>% filter(event==1) %>% mutate(dtindex=ymd(dtsortida), event=1)
  
  # Seleccionar controls i mutar dataindex en data index random 
  
  dtcontrols<-dt %>% filter(control==1) %>% mutate(dtindex=dtindex.random, event=0)
  
  # Fusionar events + controls 
  dt.total<-dtevents %>% rbind(dtcontrols)  
  
  table(dt.total$control)
  
  dt.total %>% filter(event==1 & is.na(control)) %>% arrange(idp)
  
# 3 Agregar en data index (Edat )
  
  
  
# 4 Fer matching 
  
  
  
  
  
  
  
  dt %>% select(idp,dtindex.random,dtsortida,event,variables.ps)
  
  
  columna.dataindex="dtindex"
  columna.datadortida="dtsortida"
  columnaevent="event"

  # variables.ps
  
  filter_(paste0(grup,"==1"))
  # Selecciono events i mutar dataindex (event=1) en dtindex 
  
  dtevents<-dt %>% filter_(paste0(columnaevent,"==1")) %>% mutate_("dtindex"=columna.datadortida, "event"=columnaevent)
  
  # Seleccionar controls i mutar dataindex en data index random 
  
  dtcontrols<-dt %>% filter(control==1) %>% mutate_("dtindex"=dtindex.random, event=0)
  
  dtcontrols<-dt %>% filter(control==1) %>% mutate(dtindex=dtindex.random, event=0)
  
  # Fusionar events + controls 
  dt.total<-dtevents %>% rbind(dtcontrols)
  
  
  # Agrego en dtindex 
  
  dt.total<-dt.total %>% 
    mutate (edat=as.numeric((dtindex-ymd(dnaix))/365.25))               # Calculo edat en dataindex
  
  # MatchiT 
  
  # preparar dades per matching (idp + Llista matching)
  dadesmatching<-dt.total %>% select(idp,sexe,edat,dtindex,event)
  
  #
  llistaPS<-c("sexe","edat","dtindex") # covaribles
  formulaPS<-as.formula(paste("event", paste(llistaPS, collapse=" + "), sep=" ~ "))
  
  dt.matched<-formulaPS %>% 
    matchit(method="nearest",data=dadesmatching,caliper=0.05,ratio=4) %>%    # FAig el matching 4 a 1
    weights() %>%                                                            # Guardo els pesos 
    data.table() %>% 
    'colnames<-'(c("PS")) %>% 
    bind_cols(dt.total) %>%   # Ho junto al dt.total 
    filter(PS==1)
  
  
  
  
}





  
  
  










### Aquesta funció ha de retornar una Bdades aparellada per densitat d'incicendia

matching_sequencial<-function(dt=PACIENTS,grup="EV.CI",estrat="anyindex") {
  
  dt=PACIENTS
  estrat="anyindex"
  grup="EV.CI"

  
  # Duplicar events i mutar dataindex (event=1) en dtindex.random
  dtevents<-dt %>% filter_(paste0(grup,"==1")) %>% mutate(dtindex.random=ymd(dtsortida),event=1) 
  
  # Mutar events=0 + fusionar els events
  dt<-dt %>% mutate(event=0) %>% rbind(dtevents) 
  
  set.seed(123)

  dtmatching_total<-as_tibble() ## declaro tibble on anira el matching total
  
  pepito=paste0("as.factor(dt$",estrat,")")
  llista.dades<-dt %>% split(eval(parse(text=pepito)))

  for (i in 1:length(llista.dades)) {
  
  i<-1
  
  ### fer que els casos anteriors 
  
  dt_tros<-llista.dades[[i]]
  

  ####  1. Agregar variables en data  
  ####  2. Reconvertir data index = data event en any / data index random 
  ####  3. Matxejar i seleccionar submostra a
  ####  4. Descartar els casos/controls de df i repetir-ho
  
  ### 1. Agregar 
  ### 2. Reconvertir
  
  ##### Això es clau --> Actualitzar data index segons sigui cas o no  
  ##### Recodifico EV.CI Nomes en any EV.CI=1

  ## Si Event es al mateix any que valuo elimino event
  
  condicio=paste0(grup,"==1 & year(ymd(dtsortida))==anyindex")    # genero la condicio

  dt_tros<-dt_tros %>% mutate(event=ifelse(eval(parse(text=condicio)),1,0))  # Recodifico events de periode

  dt_tros<-dt_tros %>% 
    mutate (
      dindex_temp=ifelse(event==1,as.character(dtsortida),dtindex.rand.caracter),  # ELS events -->  DATA INDEX = DATA DE CAS / DATA RANDOM
      edat=as.numeric((ymd(dindex_temp)-ymd(dnaix))/365.25),                       # Calculo edat en data index nova
      dtindex.random=ymd(dindex_temp)                                             # Actualitzo data index random
      )                       
  

  ### 3. matxejar trocet
  # preparar dades per matching (idp + Llista matching)
  dadesmatching<-dt_tros %>% select(idp,sexe,edat,dtindex.random,event)
  #
  llistaPS<-c("sexe","edat","dtindex.random") # covaribles
  formulaPS<-as.formula(paste("event", paste(llistaPS, collapse=" + "), sep=" ~ "))
  
  pomo<-formulaPS %>% 
    matchit(method="nearest",data=dadesmatching,caliper=0.05,ratio=4) %>%    # FAig el matching 4 a 1
    weights() %>%                                                            # Guardo els pesos 
    data.table() %>% 
    'colnames<-'(c("PS")) %>% 
    bind_cols(dt_tros) %>% # Ho junto al dt_tros 
    select (idp,PS,dtindex=dtindex.random,event) 
  
   # 4. Afegeixo la marca PS a DT total  

   dt<-dt %>% left_join(pomo,by="idp")

  
   # 6. Descarto els marcats amb PS de l'any a dt 

   # 5. Selecciono els matxejats (idp)
   
   dtmatching_petit<-dt %>% filter(PS!=0) %>%     ## Subset dels matxejats (PS<>0)
     select(-PS, -event)
     
   
   # 5. Acumular-los en dtmatchingtotal
   
   dtmatching_total<- dtmatching_total %>% bind_rows(dtmatching_petit)
       
  }
   
  dtmatching_total
  
}

marco<-matching_sequencial(dt=PACIENTS,grup="EV.CI",estrat="anyindex") 

table(PACIENTS$EV.CI,PACIENTS$anyindex)
table(marco$EV.CI,marco$anyindex)


PACIENTS %>% filter(year(dtindex.random)==2010)
####  Agregar variables en data (p.e edat en data) 
####  Ull perque data index = data event / data index random 

PACIENTS %>% 
  mutate (
  dindex_temp=ifelse(EV.CI==1,as.character(dtsortida.x),dtindex.rand.caracter), 
  edat=as.numeric((ymd(dindex_temp)-ymd(dnaix))/365.25)
  )

#### Ara matching per edad i sexe 

### PREPARACIO dades pel matching : 
llistaPS<-c("sexe","edat","dtindex.random") # covaribles
event<-"EV.CI"             # 
dadesmatching<-PACIENTS %>% select(idp,sexe,edat,dtindex.random,EV.CI)
formulaPS<-as.formula(paste(event, paste(llistaPS, collapse=" + "), sep=" ~ "))

##  Aplicar matching , agafar 
set.seed(123)
m.out<-formulaPS %>% matchit(method="nearest",data=dadesmatching,caliper=0.05,ratio=4) %>% 
####    comprovar el caliper (->>0.05)####
summary(m.out)
m.out

PACIENTS<-PACIENTS %>% bind_cols(data.table(weights(m.out))) %>% rename(PS=V1)

### Ara descartar el subgrup seleccionat ?
### Eliminar de taula PACIENTS eLS casos i controls seleccionats pel matching i continuar en el seguent subgrup

manel<-PACIENTS %>% filter(PS==1)
PACIENTS<-PACIENTS %>% filter(PS==0)

manel<-manel %>% bind_rows(manel)









### Ara descartar el subgrup seleccionat ?
### Eliminar de taula PACIENTS eLS casos i controls seleccionats pel matching i continuar en el seguent subgrup

PACIENTS %>% filter(PS==0)

sd(m.out$distance)*0.25
summary(m.out$distance)
summary(subset(m.out$distance,m.out$weights==1))

PS_pre<-m.out$distance
PS_post<-subset(m.out$distance,m.out$weights==1)

sd(PS_pre)*0.25
sd(PS_post)*0.25

###   afegeixo a dadestotal la variable PS 
data.table(PACIENTS,ps=m.out$weights)
#



####  descartar els casos i controls seleccionats 



####  MAtching Nc



PACIENTS %>% filter(anyindex.random==2011)

PACIENTS %>% filter(anyindex.random==2012)

PACIENTS %>% filter(anyindex.random==2013)





















