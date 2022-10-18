#####CREATE QUEJAS DATABASE##########
#####Checking the regressions before####

git config --global credential.helper store

install.packages(c("readxl", "readr", "haven", "purr", 
                   "dplyr", "estimatr"))
library(readxl)
library(readr)
library(haven)
library(purr)
library(dplyr)
library(estimatr)
services<-read_dta("services.dta")
glimpse(services)
maintenance<-lm_robust(maintenance_pc~
                         Income+participation+votos+pob+density+factor(ANY_DATA_ALTA)+factor(CODI_DISTRICTE),
                       data=services,
                       clusters=CODI_DISTRICTE,
                       se_type="stata")
summary(maintenance)
####Checking the new regressions###

quejas<-rbind(com2014, com2015, com2016, com2017, com2018,com2019)
quejas <- quejas %>% modify_if(is.character, as.factor)
distribution_types<-quejas%>%group_by(TIPUS)%>%tally()%>%ungroup()%>%arrange(desc(n))%>%mutate(percentage=n*100/1041649)

com2020<-read_csv("2020_IRIS_Peticions_Ciutadanes_OpenData (2).csv")
com2020$CODI_BARRI<-as.numeric(com2020$CODI_BARRI)
com2021<-
quejas_ipmj<-rbind(quejas,com2020 )





quejas<-quejas%>%filter(TIPUS=="INCIDENCIA" | TIPUS=="QUEIXA")%>%filter(!is.na(CODI_DISTRICTE))%>%
  select(-FITXA_ID, -TIPUS, -CODI_BARRI, -BARRI, -SECCIO_CENSAL, -TIPUS_VIA, -CARRER, -NUMERO,
         -COORDENADA_X, -COORDENADA_Y, -LONGITUD, -LATITUD, -SUPORT, -CANALS_RESPOSTA, -DIA_DATA_TANCAMENT, 
         MES_DATA_TANCAMENT, -ANY_DATA_TANCAMENT)
quejas <- quejas %>% modify_if(is.factor, as.character)
quejas <- quejas %>% modify_if(is.character, as.factor)
quejas<-quejas%>%filter(ANY_DATA_ALTA>2013

##Adding years asked from the reviewer

#cleaning complaints

View(quejas_ipmj)