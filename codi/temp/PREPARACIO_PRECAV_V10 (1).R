#              PREPARACIÓ               ----------------------------

memory.size(max=16000)

# Càrrega de dades directament de github

# library("Rurl")
# library("devtools")
 
# link<-paste0("https://github.com/jrealgatius/LLEPALI/blob/master/funcions_propies.R","?raw=T")
# devtools::source_url(link)




#
#####################  Directori Font     ==============================  

rm(list=ls())
#
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

# 0. Inicialització de parametres  -----------------------------
library(here)

fitxer_input<-"BD_PRECAV_test3.rds"
fitxer_input<-here::here("dades/preparades",fitxer_input)

output_PRECAV<-"output_PRECAV.Rdata"
output_PRECAV<-"output/R_data" %>% here::here(output_PRECAV)

conductor_variables<-"variables_precav.xls"
conductor_variables<-"dades" %>% here::here(conductor_variables)

# 1. Lectura de fitxer      ------------------

dades<-readRDS(fitxer_input) %>% as_tibble()


# 2. Calculs i Recodificacions  ----------------

library(lubridate)

##  Calcular edat 

dades<-dades %>% 
  mutate(edat = year(as.period(interval(start = ymd(dnaix), end = dtindex))))


# Farmacs 
# NA --> 0 (Else=1) (No hi ha 0'0)
dades<-dades %>% mutate_at(vars(starts_with("FP.")),
                           ~if_else(is.na(.) | .==0,0,1)) 

dades<-dades %>% mutate_at(vars(starts_with("FP1_3a.")), 
                           ~if_else(is.na(.) | .== 0,0,1)) 


# NA --> 0 (ALTRI ES UNA COPIA)
dades<-dades %>% mutate_at(vars(starts_with("FD.")), 
                           ~if_else(is.na(.) | .==0,0,1)) 

# NA --> 0 (ALTRI ES UNA COPIA)
dades<-dades %>% mutate_at(vars(starts_with("FD1_3a.")), 
                           ~if_else(is.na(.) | .==0,0,1)) 

# Diagnostics
# NA --> 0 (Else=1) (No hi ha 0)
dades<-dades %>% mutate_at(vars(starts_with("DG.")), 
                           ~if_else(is.na(.) | .==0,0,1)) 

# Diagnostics (ANTECEDENTS)
# NA --> 0 (Else=1) (No hi ha 0)
dades<-dades %>% mutate_at(vars(starts_with("ANT1_3.")), 
                           ~if_else(is.na(.) | .==0,0,1)) 

# Events
dades<-dades %>% mutate_at(vars(starts_with("EV.")), 
                           ~if_else(is.na(.) | .==0,0,1) ) 

# Càlcul de número d'events Terciaris

dades<-dades %>% mutate(
  EV.TER_SUM = rowSums(select(., starts_with("EV.TER."))))

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
                           ~if_else(is.na(.) | .==0,0,.) )

# Recodes
dades<-dades %>% 
  mutate(edat_pre=ifelse(sexe=="H" & edat<55,"Young man(<55)",NA)) %>% 
  mutate(edat_pre=ifelse(sexe=="H" & edat>=55,"Old man(>=55)",edat_pre)) %>% 
  mutate(edat_pre=ifelse(sexe=="D" & edat<65,"Young woman(<65)",edat_pre)) %>% 
  mutate(edat_pre=ifelse(sexe=="D" & edat>=65,"Old woman(>=65)",edat_pre)) 

# Recodes
dades<-dades %>% 
  mutate(edat_grup=ifelse((sexe=="H" & edat<55) | (sexe=="D" & edat<65) ,"Young:M<55/F<65",NA)) %>% 
  mutate(edat_grup=ifelse((sexe=="H" & edat>=55) | (sexe=="D" & edat>=65),"Old:M>=55/F>=65",edat_grup))  

# Recodes
dades<-dades %>% 
  mutate(any_index=factor(lubridate::year(dtindex)))

# REGICOR  ------------------

# Selecciono variables REGICOR i formatejo 
# Ojo que s'ha de recalcular la edat 1-3 anys previs 

dades_regicor<-selectorvariables("regicor",conductor_variables,dades) %>% 
  mutate(age=edat,
         diabetes=if_else(ANT1_3.E10==1 | ANT1_3.E11,1,0),
         smoker=if_else(tab.valor==1,1,0,missing = 0), 
         coltot=COLTOT.valor13a,
         colhdl=COLHDL.valor13a,
         sbp=PAS.valor13a,
         dbp=PAD.valor13a) 
  

# Aplico formula i ho fuciono a dades
temp<-dades_regicor
regicor_df<-regicor(temp$age,temp$sexe,temp$smoker,temp$diabetes,temp$coltot,temp$colhdl,temp$sbp,temp$dbp) 

regicor_df<-tibble(regicor_df) %>% rename(regicor=regicor_df)

dades<-dades %>% bind_cols(regicor_df)

rm(temp,dades_regicor,regicor_df)


# Variables Emilio Ortega --------------

# HDL BAJO:  hdlbajo13a_cat2 (1-3a=ultims 3 anys)

dades<-dades %>% 
  mutate(hdlbajo13a_cat2=case_when(
    sexe=="H" & COLHDL.valor13a < 40 ~ "Si",
    sexe=="D" & COLHDL.valor13a < 50 ~ "Si",
    sexe=="H" & COLHDL.valor13a >=40 ~ "No",
    sexe=="D" & COLHDL.valor13a >=50 ~ "No"))

# Dislipemia aterogénica disl13a_cat2  (Any Missings = missings)

dades<-dades %>% 
  mutate(disl13a_cat2=case_when(
    TG.valor13a>200 & hdlbajo13a_cat2=="Si"~ "Si",
    TG.valor13a>200 & hdlbajo13a_cat2=="No"~ "No",
    TG.valor13a<=200 & hdlbajo13a_cat2=="Si"~ "No",
    TG.valor13a<=200 & hdlbajo13a_cat2=="No"~ "No")) 
                 
# Colesterol remanente: col_rema13a
dades<-dades %>% mutate(col_rema13a=COLTOT.valor13a-COLLDL.valor13a-COLHDL.valor13a) 

# Colesterol noHDL: col_noHDL13a 

dades<-dades %>% mutate(col_noHDL13a=COLTOT.valor13a--COLHDL.valor13a) 

# regicor_cat (DM1 no te regicor)
dades<-dades %>% mutate(regicor_cat=case_when(
  DG.E10==0 & regicor<4.51~1,
  DG.E10==0 & regicor>=4.51 & regicor<=9.51~2,
  DG.E10==0 & regicor>=9.51 & regicor<=14.51~3,
  DG.E10==0 & regicor>=14.51 ~4)) 

# Recodes

dades<-recodificar(dades,taulavariables = conductor_variables,criteris = "recodes")


dades$event %>% table()

# 2.1. Filtre por edat (Criteri d'inclusió entre 35 y 75 anys)         ------------------------------

dades<-dades %>% filter(edat>=35 & edat<75)
events_dt <-dades %>% filter(event==1) 
events_dt %>% skimr::skim(edat)

# Etiquetació / factorització   ------------------------------


# Factoritzar NO.Yes llista de variables "factor" situades a la taulavariables camp=factor
dades<-factoritzar.NO.YES(dt=dades,columna="factor.YESNO",taulavariables=conductor_variables)

# Value labels
dades<-etiquetar_valors(dades,variables_factors = conductor_variables,fulla="value_label")

# Etiquetar 
dades<-etiquetar(d=dades,taulavariables = conductor_variables)

# Generar Flow-chart --------------------
pob_lab<-"Población inicial (8%)"
pob_lab1<-c("Potenciales controles","Controles")
pob_lab2=c("Potenciales casos","Casos")
pob<-c(287063)
pob1<-c(273459,38022)
exc1<-c(229982,5455)
exc_lab1<-c('Not matching','Fuera rango edad')
pob2<-c(13604,11367)
exc2<-c(0,2237)
exc_lab2<-c("Not matching","Fuera rango edad(35-75)")

flow_chart<-diagramaFlowchart2G(pob_lab=pob_lab,pob=pob,pob_lab1=pob_lab1,pob_lab2=pob_lab2,pob1=pob1,exc1=exc1,exc_lab1=exc_lab1,pob2=pob2,exc2=exc2,exc_lab2=exc_lab2)



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
# Tecnica per no arrossegar tota la base de dades
formula<-formula_compare(x="events",y="event",taulavariables = conductor_variables) 
dades_temp<-dades %>% select(extreure.variables("events",conductor_variables),"event")
T2.EVENTS<-descrTable(formula,data=dades_temp,show.p.overall = F,hide.no = "No")

formula<-formula_compare(x="farmacs_previs",y="event",taulavariables = conductor_variables) 
dades_temp<-dades %>% select(extreure.variables("farmacs_previs",conductor_variables),"event")
T2.FARMACS<-descrTable(formula,data=dades_temp,show.p.overall = F,hide.no = "No")

formula<-formula_compare(x="farmacs",y="event",taulavariables = conductor_variables) 
dades_temp<-dades %>% select(extreure.variables("farmacs",conductor_variables),"event")
T2.FARMACS_ACTUALS<-descrTable(formula,data=dades_temp,show.p.overall = F,hide.no = "No")

formula<-formula_compare(x="antecedents_previs",y="event",taulavariables = conductor_variables) 
dades_temp<-dades %>% select(extreure.variables("antecedents_previs",conductor_variables),"event")
T2.ANTECEDENTS<-descrTable(formula,data=dades_temp,show.p.overall = F,hide.no = "No",show.n = T)

formula<-formula_compare(x="valors_previs",y="event",taulavariables = conductor_variables) 
dades_temp<-dades %>% select(extreure.variables("valors_previs",conductor_variables),"event")
T2.VALORS<-descrTable(formula,data=dades_temp,show.p.overall = F,hide.no = "No",show.n = T)

formula<-formula_compare(x="regicor",y="event",taulavariables = conductor_variables) 
dades_temp<-dades %>% select(extreure.variables("regicor",conductor_variables),"event")
T2.regicor<-descrTable(formula,data=dades_temp,show.p.overall = F,hide.no = "No",show.n = T,show.ratio = T)

# Comparar diferencias observadas por grupo de edad (edat_grup) -------------
formula<-formula_compare(x="regicor",y="event",taulavariables = conductor_variables) 
dades_temp<-dades %>% filter(edat_grup=="Young:M<55/F<65") %>% select(extreure.variables("regicor",conductor_variables),"event")
T2.regicor_g1<-descrTable(formula,data=dades_temp,show.p.overall = F,hide.no = "No",show.n = T,show.ratio = T)

formula<-formula_compare(x="regicor",y="event",taulavariables = conductor_variables) 
dades_temp<-dades %>% filter(edat_grup=="Old:M>=55/F>=65") %>% select(extreure.variables("regicor",conductor_variables),"event")
T2.regicor_g2<-descrTable(formula,data=dades_temp,show.p.overall = F,hide.no = "No",show.n = T,show.ratio = T)

# Evaluar Regicor GLOBAL -----------------
dades_temp<-dades

# LOGISTICA CONDICIONAL ---------------

M1.reg.alon.ls<-extreure_model_logistic(x="regicor_alone",y="event",taulavariables=conductor_variables,dades=dades_temp,valor_outcome=1,conditional = T,strata = "caseid")
M2.reg.vars.ls<-extreure_model_logistic(x="regicor_vars",y="event",taulavariables=conductor_variables,dades=dades_temp,valor_outcome=1,conditional = T,strata = "caseid")
M3.reg.plus.ls<-extreure_model_logistic(x="regicor_plus",y="event",taulavariables=conductor_variables,dades=dades_temp,valor_outcome=1,conditional = T,strata = "caseid")

# Evaluar Regicor GLOBAL / Subgrups  -----------------
library(plotROC)
ROC.reg.alone.plot<-ggplot(dades, aes(d = event, m = regicor,color=edat_grup)) + 
  plotROC::geom_roc(n.cuts = 0) + 
  plotROC::style_roc()

ROC.reg.alone.plot <- ROC.reg.alone.plot + style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  ggtitle("ROC Curve") +
  annotate("text", x = .75, y = .25,
             label = paste("AUC =", round(plotROC::calc_auc(ROC.reg.alone.plot)["AUC"], 2))) 

# Evaluar Regicor G1
dades_temp<-dades %>% filter(edat_grup=="Young:M<55/F<65")
M1.reg.alon.G1.ls<-extreure_model_logistic(x="regicor_alone",y="event",taulavariables=conductor_variables,dades=dades_temp,valor_outcome=1,conditional = T,strata = "caseid")
M2.reg.vars.G1.ls<-extreure_model_logistic(x="regicor_vars",y="event",taulavariables=conductor_variables,dades=dades_temp,valor_outcome=1,conditional = T,strata = "caseid")
M3.reg.plus.G1.ls<-extreure_model_logistic(x="regicor_plus",y="event",taulavariables=conductor_variables,dades=dades_temp,valor_outcome=1,conditional = T,strata = "caseid")

# Evaluar Regicor G2
dades_temp<-dades %>% filter(edat_grup=="Old:M>=55/F>=65")
M1.reg.alon.G2.ls<-extreure_model_logistic(x="regicor_alone",y="event",taulavariables=conductor_variables,dades=dades_temp,valor_outcome=1,conditional = T,strata = "caseid")
M2.reg.vars.G2.ls<-extreure_model_logistic(x="regicor_vars",y="event",taulavariables=conductor_variables,dades=dades_temp,valor_outcome=1,conditional = T,strata = "caseid")
M3.reg.plus.G2.ls<-extreure_model_logistic(x="regicor_plus",y="event",taulavariables=conductor_variables,dades=dades_temp,valor_outcome=1,conditional = T,strata = "caseid")

## Salvar objectes creats -------------------

rm(list=c("events_dt","dadesevents","dades","dades_temp"))

gc()

save.image(file=output_PRECAV)

# save(T1.EVENTS,T1.2.EVENTS,T1.3.EVENTS,T1.FARMACS,T1.BASELINE,T2.EVENTS,T2.FARMACS,T2.BASELINE
#      ,file=output_PRECAV)



