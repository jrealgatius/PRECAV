####              PREPARACIÓ               ----------------------------


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


fitxer_input<-"BD_PRECAV.rds"

fitxer_output<-"BD_PRECAV_N"


# 1. Lectura de fitxer      ------------------

dades<-readRDS(fitxer_input) %>% as_tibble()


# 2. Calculs i Recodificacions  ----------------

library(lubridate)




##  Calcular edat 

dades<-dades %>% 
  mutate(edat = year(as.period(interval(start = ymd(dnaix), end = dataindex))))

# Farmacs 
# NA --> 0
dades<-dades %>% mutate_at(vars(starts_with("FP.")), 
          funs(if_else(is.na(.),0,1) ) 
          ) 

# Diagnostics
dades<-dades %>% mutate_at(vars(starts_with("DG.")), 
                 funs(if_else(is.na(.),0,1) ) 
) 

# Events
dades<-dades %>% mutate_at(vars(starts_with("EV.")), 
                 funs(if_else(is.na(.),0,1) ) 
) 

# visites 
dades<-dades %>% mutate_at(vars(starts_with("visites_")), 
                           funs(if_else(is.na(.),0,.) ))

# Recodes
dades<-dades %>% 
  mutate(edat_pre=ifelse(sexe=="H" & edat<55,"Young man",NA)) %>% 
  mutate(edat_pre=ifelse(sexe=="H" & edat>=55,"Old man",edat_pre)) %>% 
  mutate(edat_pre=ifelse(sexe=="D" & edat<65,"Young woman",edat_pre)) %>% 
  mutate(edat_pre=ifelse(sexe=="D" & edat>=65,"Old woman",edat_pre)) 
  

# dades<-mutate_at(dades,vars(starts_with("FP.")), 
#                  funs(bin=if_else(is.na(.),0,1) ) 
#                  ) 

# ANALISIS PRELIMINAR 






