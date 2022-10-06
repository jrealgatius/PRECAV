####              PREPARACIÓ               ----------------------------

memory.size(max=16000)


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

output_PRECAV<-"output_PRECAV.Rdata"

output_PRECAV

conductor_variables<-"variables_precav.xls"


# 1. Lectura de fitxer      ------------------

dades<-readRDS(fitxer_input) %>% as_tibble()


# 2. Calculs i Recodificacions  ----------------

library(lubridate)

##  Calcular edat 

dades<-dades %>% 
  mutate(edat = year(as.period(interval(start = ymd(dnaix), end = dtindex))))


# Farmacs 
# NA --> 0 (Else=1) (No hi ha 0)
dades<-dades %>% mutate_at(vars(starts_with("FP.")), 
          funs(if_else(is.na(.),0,1) ) 
          ) 

# NA --> 0 (ALTRI ES UNA COPIA)
dades<-dades %>% mutate_at(vars(starts_with("FD.")), 
                           funs(if_else(is.na(.),0,.) )
                           ) 
# Diagnostics
# NA --> 0 (Else=1) (No hi ha 0)
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
  mutate(edat_pre=ifelse(sexe=="H" & edat<55,"Young man(<55)",NA)) %>% 
  mutate(edat_pre=ifelse(sexe=="H" & edat>=55,"Old man(>=55)",edat_pre)) %>% 
  mutate(edat_pre=ifelse(sexe=="D" & edat<65,"Young woman(<65)",edat_pre)) %>% 
  mutate(edat_pre=ifelse(sexe=="D" & edat>=65,"Old woman(>=65)",edat_pre)) 

# Recodes
dades<-dades %>% 
  mutate(edat_grup=ifelse((sexe=="H" & edat<55) | (sexe=="D" & edat<65) ,"Young:M<55/F<65",NA)) %>% 
  mutate(edat_grup=ifelse((sexe=="H" & edat>=55) | (sexe=="D" & edat>=65),"Old:M>=65/F>=65",edat_grup))  

# Recodes
dades<-dades %>% 
  mutate(any_index=factor(lubridate::year(dtindex)))

# 2.1. Filtre por edat    ----------------

events_dt <-dades %>% filter(event==1) 

events_dt %>% skimr::skim(edat)

events_dt<-events_dt %>% filter(edat>=35 & edat<75)



# Factoritzar 


# dades<-mutate_at(dades,vars(starts_with("FP.")), 
#                  funs(bin=if_else(is.na(.),0,1) ) 
#                  ) 

# Etiquetació / factorització ------------------------------

##########      factoritzar NO.Yes llista de variables "factor" situades a la taulavariables camp=factor
dades<-factoritzar.NO.YES(dt=dades,columna="factor.YESNO",taulavariables=conductor_variables)

dades<-etiquetar(d=dades,taulavariables = conductor_variables)

# ANALISIS PRELIMINAR (Descripció dels events)  ------------------------
dadesevents<-dades %>% filter(event==1)

paco<-dadesevents %>% group_by(codindex) %>% count()

dadesevents %>% filter(EV.TER.ARTER_PERIF + EV.TER.AVC + EV.TER.CI + EV.TER.INSF_CARD)

paco<-extreure.variables("events",conductor_variables)

dadesevents<-comptar_valors(dadesevents,paco,valor="Yes")

table(dadesevents$num_valors)


T0.EVENTS<-descrTable(formula_kk,data=dadesevents,show.p.overall = F,hide.no = "No")

formula<-formula_compare(x="events",y="",taulavariables = conductor_variables) 
T1.EVENTS<-descrTable(formula,data=dadesevents,show.p.overall = F,hide.no = "No")

formula<-formula_compare(x="farmacs",y="",taulavariables = conductor_variables) 
T1.FARMACS<-descrTable(formula,data=dadesevents,show.p.overall = F,hide.no = "No")

formula<-formula_compare(x="baseline",y="",taulavariables = conductor_variables) 
T1.BASELINE<-descrTable(formula,data=dadesevents,show.p.overall = F,hide.no = "No")


# Comparativa amb controls  -----------------


formula<-formula_compare(x="events",y="event",taulavariables = conductor_variables) 
T2.EVENTS<-descrTable(formula,data=dades,show.p.overall = F,hide.no = "No")

formula<-formula_compare(x="farmacs",y="event",taulavariables = conductor_variables) 
T2.FARMACS<-descrTable(formula,data=dades,show.p.overall = F,hide.no = "No")

formula<-formula_compare(x="baseline",y="event",taulavariables = conductor_variables) 
T2.BASELINE<-descrTable(formula,data=dades,show.p.overall = F,hide.no = "No",show.n = T)


## Salvar objectes creats -------------------


save(T1.EVENTS,T1.FARMACS,T1.BASELINE,T2.EVENTS,T2.FARMACS,T2.BASELINE,file=output_PRECAV)


# filtrar per grups de risk que el seu control te algun event CV ? 



