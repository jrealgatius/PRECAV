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
                           funs(if_else(is.na(.),0,1)) 
                            ) 

# Events
dades<-dades %>% mutate_at(vars(starts_with("EV.")), 
                 funs(if_else(is.na(.),0,1) ) 
                 ) 

# Càlcul de número d'events Terciaris

dades<-dades %>% mutate(
  EV.TER_SUM = rowSums(select(., starts_with("EV.TER.")))
  )

# Recode EV.TER_SUM

dades<-dades %>% mutate(
  EV.TER_multi = case_when(
    EV.TER_SUM==1~"1 terr",
    EV.TER_SUM>1~">=2 terr")
  )

# Càlcul de número d'events tipus

dades<-dades %>% mutate(
  EV.TIP_SUM = rowSums(select(., starts_with("EV.TIP.")))
  )

# Recode EV.TIP_SUM

dades<-dades %>% mutate(
  EV.TIP_multi = case_when(
  EV.TIP_SUM==1~"1 ECV",
  EV.TIP_SUM>1~">=2 ECVs")
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

# 2.1. Filtre por edat (Criteri d'inclusió entre 35 y 75 anys)         ------------------------------

dades<-dades %>% filter(edat>=35 & edat<75)
events_dt <-dades %>% filter(event==1) 
events_dt %>% skimr::skim(edat)

# Etiquetació / factorització   ------------------------------

##########      factoritzar NO.Yes llista de variables "factor" situades a la taulavariables camp=factor
dades<-factoritzar.NO.YES(dt=dades,columna="factor.YESNO",taulavariables=conductor_variables)

dades<-etiquetar(d=dades,taulavariables = conductor_variables)

# ANALISIS PRELIMINAR (Descripció dels events)  ------------------------
dadesevents<-dades %>% filter(event==1)

formula<-formula_compare(x="events",y="",taulavariables = conductor_variables) 
T1.EVENTS<-descrTable(formula,data=dadesevents,show.p.overall = F,hide.no = "No")

formula<-formula_compare(x="events_ter",y="",taulavariables = conductor_variables) 
T1.2.EVENTS<-descrTable(formula,data=dadesevents,show.p.overall = F,hide.no = "No")

formula<-formula_compare(x="events_imp",y="",taulavariables = conductor_variables) 
T1.3.EVENTS<-descrTable(formula,data=dadesevents,show.p.overall = F,hide.no = "No")

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

save(T1.EVENTS,T1.2.EVENTS,T1.3.EVENTS,T1.FARMACS,T1.BASELINE,T2.EVENTS,T2.FARMACS,T2.BASELINE,file=output_PRECAV)









