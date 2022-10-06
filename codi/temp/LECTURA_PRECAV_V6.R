#########################     LECTURA DE DADES          #################################
#
####  Aplicació de criteris d'inclusió i lectura de dades     ###########################

memory.size(max=130685)


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

"CIBERDEM/GEDAPS/PRECAV/DADESSIDIAP2" %>% 
  directori_treball(directori.arrel)


####################################     LLISTA DE FITXERS        #########################

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

###########################################################################

########      LESTURA DE DADES      #############################

### LECTURA ######

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

library("xlsx")
write.xlsx(CATALEG,file="cataleg.xlsx")

#########################################################################################
#


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

### mostra de 1000000
PROBLEMES_total <- PROBLEMES_total[1:100000]


# Agrego problemes amb límit data máxima (31/12/2016) O sigui dels diagnostics agafo la primera data de cada problema de salut


#### Fusiono agregador (+agr) 

dates_casos<-PROBLEMES_total %>% 
  agregar_problemes(bd.dindex="20161231",dt.agregadors=CATALEG) %>%   ## agrego problemes de salut
  select(idp, CARD_ISQ) %>%         ## Selecciono CARD_ISQ que serà la data index dels events 
  drop_na() %>%                     ## borro missing
  ungroup()


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


##    Algoritme de selecció de controls
##
##   0.	Assignar una data de sortida (data màxima a risc de tenir un esdeveniment)
##        	31/12/2016 / Data de mort  / Data de primer esdeveniment CV)

# EV.CI --> (Event) data sortida = data event
# Defunció data sortida = Data defuncio última de seguiment (12/2016)
# altrament --> 20161231

PACIENTS <- PACIENTS %>% 
  mutate (dtsortida=ifelse(situacio=="D",sortida,20161231)) %>%     ### Difunts ---> Data sortida
  mutate (dtsortida=ifelse(event==1,CARD_ISQ,dtsortida))            ### Events data de sortida (CARD_ISQ)



#####################################       APARELLAMENT by riskSetMatch                         ####################
################               Preparo la base de dades per l'aparellamenta                    #####################


#######  Hi funsiono data random en a pacients       ######
dt=PACIENTS


# dt <-dt %>% 
#   left_join(BD_PAC_DINDEX,by="idp") %>% select(-CARD_ISQ)

######
######          dtindex_case = Data event
dt<-dt %>% mutate(dtindex_case=ifelse
                  (event==1, as.integer(lubridate::ymd(dtsortida)),NA))

#### Data de censura en controls (dtsortida)   dtindex_control

dt<-dt %>% mutate (dtindex_control=as.integer(lubridate::ymd(dtsortida)))

####  Genero any de naixament 

dt<-dt %>% mutate (
  any_naix=lubridate::year(lubridate::ymd(dnaix)),
  any_naix_g10=Hmisc::cut2(any_naix,g=10),
  any_naix_g17=Hmisc::cut2(any_naix,seq(from = 1935, to = 1974, by = 2))
) 


#################################     FUNCIÓ riskSetMatch           ################ 
### Previament HE D'INSTALAR heaven 
# install.packages("devtools")
# install.packages("githubinstall")
# library(githubinstall)
# githubinstall("heaven")

library(heaven)

#### Llista d'aparellamenta

names(dt)

llistaPS=c("sexe","any_naix","idup")


pp<-riskSetMatch(ptid="idp"                                # Unique patient identifier
                 ,event="event"                # 0=Control, 1=case
                 ,terms=llistaPS   # terms c("n1","n2",...) - list of vairables to match by
                 ,dat=dt                       # dataset with all variables
                 ,Ncontrols=20                  # number of controls to provide
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

pp

matchReport(pp, "idp",case="event",caseid="caseid")

pp[,numControls:=.N,by=caseid]

pp %>% group_by(caseid) %>% summarise(n=n()) %>% summarise(mean(n),min(n),max(n))

dt %>% distinct(idp)

################      COMPROVACIÓ DE MATCHING PP        ###################

names(pp)

susu<-pp %>% filter(oldevent==1 & event==0)

table(pp$event,pp$oldevent)
table(pp$event)
table(pp$oldevent)


llistaPS=c("sexe","any_naix")

llista.compare.Ys(dt=pp,llista.y=c("event"),llista.x=llistaPS,show.ratio=F,byrow=F)



########################################################################################################

pp<-pp

LELO<-agregar_visites(dt=VISITES[1:200000],bd.dindex = pp)

















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

# VARIABLES<-fread('ECV_CAT_entregable_variables_cliniques_20180122_150054.txt',sep="|")


###################     A) APLICO MATCHING UTILITZANT data_index random           ###################

# 1 Genero data random 
# 1 Generar data index en potencials controls (inclou events futurs)

BD_PAC_DINDEX<-dt_index_data_random(PACIENTS)

BD_PAC_DINDEX <- BD_PAC_DINDEX %>% mutate(control=1)

llistaPS<-c("sexe","edat","dtindex") # covaribles

# 2 Aplico matching a PACIENTS amb covariables variables.ps amb dades amb data index --> BD_PAC_DINDEX


dades_matching<-matching_case_control(dt=PACIENTS,variables.ps=llistaPS,dt_pacients_dindex=BD_PAC_DINDEX) 

## Comprovo aparellament 

llista.compare.Ys(dt=dades_matching,llista.y=c("event"),llista.x=llistaPS,show.ratio=F,byrow=F)


###################    B) APLICO MATCHING UTILITZANT data_index semi random           ###################

###  Selecciono data d'analitica semi random dins del periode de seguiment 

bdades.dades.semirandom<-dt_index_data_semirandom(dt=PACIENTS,dt.variables=VARIABLES,codi="EK201")

#  Preparo base de dades 
bdades.dades.semirandom<-bdades.dades.semirandom %>% 
  mutate(dtindex.random =lubridate::ymd(dtindex.semirandom), # Converteixo en format data 
         control=1) %>%                       # Genero grup control
  select (idp,dtindex.random, control)        # Camps mínims que necessito
#

llistaPS<-c("sexe","edat","dtindex") # covaribles

dades_matching_semirandom<-matching_case_control(dt=PACIENTS,variables.ps=llistaPS,bdades.dades.semirandom) 

## Comprovo aparellament 

llista.compare.Ys(dt=dades_matching_semirandom,llista.y=c("event"),llista.x=llistaPS,show.ratio=F,byrow=F)


#########################################         FI MATCHING                                     #########################


####                                AGREGACIÓ DE VARIABLES EN DATA INDEX                          #######################################

#### VARIABLES      ##### 

# VARIABLES_SUB<-VARIABLES[1:1000000,]

#### Selecciono les variables que necessito per passar-ho al agregate <idp , dataindex>

BDINDEX<-dades_matching %>% 
  mutate (dataindex=data.to.string(dtindex)) %>% 
  select(idp,dataindex) 


###     Agrego variables en una finestra de 365 dies previ a data index   ################

variables_agregades<-agregar_analitiques(dt=VARIABLES,finestra.dies=365,bd.dindex=BDINDEX)


###                                 AGREGACIÓ DE PROBLEMES DE SALUT (Antecedents)               #

diagnostics %>% agregar_problemes(dt.agregadors=CATALEG,bd.dindex=BDINDEX)

dt_antecedents<-diagnostics %>% agregar_problemes(dt.agregadors=CATALEG,bd.dindex=BDINDEX)


###                                 AGREGACIÓ PRESCRIPCIONS 







#######################   PROVA     ###############################

library("dplyr")
library("lubridate")




































##########################################    Exemple           ############################################




case <- c(rep(0,40),rep(1,15))
ptid <- paste0("P",1:55)
sex <- c(rep("fem",20),rep("mal",20),rep("fem",8),rep("mal",7))
byear <- c(rep(c(2020,2030),20),rep(2020,7),rep(2030,8))
case.Index <- c(seq(1,40,1),seq(5,47,3))
control.Index <- case.Index
diabetes <- seq(2,110,2)
heartdis <- seq(110,2,-2)
diabetes <- c(rep(1,55))
heartdis <- c(rep(100,55))

dat <- data.table(case,ptid,sex,byear,diabetes,heartdis,case.Index,control.Index)


# Very simple match without reuse - no dates to control for


out <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,NoIndex=TRUE)


# Risk set matching without reusing cases/controls - 
#' # Some cases have no controls
out2 <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex="case.Index",controlIndex="control.Index")

out2[]



# Risk set matching with reuse of cases (control prior to case) and reuse of 
# controls - more cases get controls
out3 <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex="case.Index",controlIndex="control.Index",
                     reuseCases=TRUE,reuseControls=TRUE)

out3[]

out5 <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex=
  "case.Index",controlIndex="control.Index"
  ,reuseCases=TRUE,reuseControls=TRUE,cores=1,
  dateterms=c("diabetes","heartdis"))

out5[]

# POSTPROCESSING
# It may be convinient to add the number of controls found to each case in or-
# der to remove cases without controls or where very few controls have been
# found.  This is easily obtained using data.table - with the example above:

out5[,numControls:=.N,by=caseid] # adds a column with the number of controls

out5 <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex=
                       "case.Index",controlIndex="control.Index"
                     ,reuseCases=TRUE,reuseControls=TRUE,cores=1,
                     dateterms=c("diabetes","heartdis"))


# Es neceseciten camps com <dtsortida idp event> + llista de variables a matxejar
# <idp, dtindex.random, control> en BD_PAC_DINDEX 

# 2 Fusionar events i controls en una sola taula

dt <-dt %>% 
  left_join(dt_pacients_dindex,by="idp")   


dadesmatching



dadesmatching<-dadesmatching %>% mutate (d0=lubridate::ymd("20100101"),
                          dies=dtindex-d0) 
dadesmatching<-dadesmatching %>% mutate (edat_num=round(edat,0))


dades_test_match<-ccwc(entry=0, exit=dtindex, fail=event, origin=0, controls=4, match=list(sexe,edat_num),data=dadesmatching) 



dades_test_match<-ccwc(entry=0, exit=dtindex, fail=event, origin=0, controls=1, match=list(edat,edat),data=dadesmatching,include = list(idp)) 

names(dades_test_match$Set)

dades_test_match$Map
class(dades_test_match)

PACIENTS

install.packages("Epi")
library(Epi)

ccwc(doe, dox, chd, origin=dob, controls=2, data=diet,
     include=energy, match=job)

data(diet)

table(diet$chd)

dietcc <- ccwc(doe, dox, chd, origin=dob, controls=2, data=diet,
               include=energy, match=job)


install.packages("riskSetMatch")
library(riskSetMatch)
