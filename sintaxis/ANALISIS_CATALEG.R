# Analisis d'agregadors ----------------


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


# fitxer conductor cataleg 
fitxer_conductor_cataleg<-"cataleg_definitiu 27.1.19.xls"

# Lectura cataleg     ---------------
CATALEG<-readxl::read_excel(fitxer_conductor_cataleg)

# Elimino missings en agregadors 
CATALEG<-CATALEG %>% mutate(num.na=rowSums(is.na(.))) %>% filter(num.na<6)

# Selecciono dominis de problemes de salut 

CATALEG<-CATALEG %>% filter(domini=="cmbdh_diagnostics" | 
                     domini=="cmbdh_diagnostics_padris"|
                     domini=="cmbdh_procediments_padris"|
                     domini=="diagnostics")

# Detectar que tots els AGR_TER estan a AGR2

# Elimino duplicats amb els mateixos agregadors 

unics<-CATALEG %>% group_by(cod,agr,AGR2,AGR_PR,AGR_AMP,AGR_SEC,AGR_TER) %>% slice(1) %>% ungroup()

table(unics$AGR2)


sum(table(unics$AGR2))


# Número de codis per fila

# Número de controls per conjut a risk 
unics<-data.table(unics)
unics[,numCodis:=.N,by=cod]

# Reemplaça missings per text en blanc
unics<-unics %>% replace(is.na(.), "")



write.csv2(unics,file="cataleg_unics.txt")

write.csv2(paste0('"=""', unics, '"""'), 'unics.txt', row.names = F, quote = F)

unics %>% distinct(cod)

table(unics$AGR2)

table(unics$cod,unics$AGR2)

table(unics$agr)
table(unics$AGR2)
table(unics$AGR2,unics$AGR_PR)
table(unics$AGR2,unics$AGR_AMP)
table(unics$AGR2,unics$AGR_SEC)

## Selecciono només Events
PP<-unics %>% filter(AGR_TER=="ECV_TER")

table(PP$agr,PP$AGR2)

table(unics$agr,unics$AGR2)

## Estudi d'events -----------------

# 1. Lectura de fitxer      ------------------
fitxer_input<-"BD_PRECAV.rds"

dades<-readRDS(fitxer_input) %>% as_tibble()

events_dt<-dades %>% filter(event==1)

class(events_dt)


subevents<-events_dt %>% select(idp,dtindex,codindex,EV.ANGOR,EV.ARTER_PERIF,EV.AVC,EV.CI_INDET,EV.IAM,EV.INSF_CARD,
                                EV.RVC,EV.RVP)

papa<-subevents %>% mutate(num.na=rowSums(!is.na(.)))

table(papa$num.na)



##  Calcular edat 
library(lubridate)
events_dt<-events_dt %>% 
  mutate(edat = year(lubridate::as.period(interval(start = ymd(dnaix), end = dtindex))))



install.packages("skimr")

skimr::skim()




# Recodes
events_dt<-events_dt %>% 
  mutate(edat_pre=ifelse(sexe=="H" & edat<55,"Young man(<55)",NA)) %>% 
  mutate(edat_pre=ifelse(sexe=="H" & edat>=55,"Old man(>=55)",edat_pre)) %>% 
  mutate(edat_pre=ifelse(sexe=="D" & edat<65,"Young woman(<65)",edat_pre)) %>% 
  mutate(edat_pre=ifelse(sexe=="D" & edat>=65,"Old woman(>=65)",edat_pre)) 

# Recodes
events_dt<-events_dt %>% 
  mutate(edat_grup=ifelse((sexe=="H" & edat<55) | (sexe=="D" & edat<65) ,"Young:M<55/F<65",NA)) %>% 
  mutate(edat_grup=ifelse((sexe=="H" & edat>=55) | (sexe=="D" & edat>=65),"Old:M>=55/F>=65",edat_grup))  


table(events_dt$edat_pre)

table(events_dt$edat_grup)


descrTable(~edat_pre+edat_grup,data=events_dt)






