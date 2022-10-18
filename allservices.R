#####CREATE QUEJAS DATABASE##########
library(readr)


quejas<-rbind(com2014, com2015, com2016, com2017, com2018,com2019)
library(purrr)
quejas <- quejas %>% modify_if(is.character, as.factor)
quejas<-quejas%>%filter(TIPUS=="INCIDENCIA" | TIPUS=="QUEIXA")%>%filter(!is.na(CODI_DISTRICTE))%>%
  select(-FITXA_ID, -TIPUS, -CODI_BARRI, -BARRI, -SECCIO_CENSAL, -TIPUS_VIA, -CARRER, -NUMERO,
         -COORDENADA_X, -COORDENADA_Y, -LONGITUD, -LATITUD, -SUPORT, -CANALS_RESPOSTA, -DIA_DATA_TANCAMENT, 
         MES_DATA_TANCAMENT, -ANY_DATA_TANCAMENT)
quejas <- quejas %>% modify_if(is.factor, as.character)
quejas <- quejas %>% modify_if(is.character, as.factor)
quejas<-quejas%>%filter(ANY_DATA_ALTA>2013)
library(stringr)
quejas$AREA<-as.character(quejas$AREA)
quejas$AREA2<-ifelse(quejas$AREA!="Recollida i neteja de l'espai urbà",quejas$AREA,
                     ifelse(quejas$AREA=="Recollida i neteja de l'espai urbà"&
                              str_detect(quejas$DETALL,"Sacs|Retirada|Recollida|retirar|Escombrat|recollida|Contenidors|Contenidor|recollits|
                                        recollida|buidar|recolida|sacs"), "Recollida", "Neteja" ))


distribution<-quejas%>%group_by(AREA2)%>%tally()%>%ungroup()%>%arrange(desc(n))
library(openxlsx)
library(tidyr)
write.xlsx(distribution, "distribution3.xlsx")
library(tidyr)
services_district<-quejas%>%group_by(AREA2, MES_DATA_ALTA, CODI_DISTRICTE, ANY_DATA_ALTA)%>%
  tally()%>%ungroup()%>%
  complete(expand(quejas, CODI_DISTRICTE=1:10, ANY_DATA_ALTA=2014:2019, MES_DATA_ALTA=1:12,AREA2),
                              fill=list(n=0))
View(services_district)
services_wide<-spread(services_district, AREA2, n)%>%
  mutate(culture=Cultura,
         waste=Recollida,
         cleaning=Neteja,
         info=`Informació, tràmits i atenció al ciutadà`,
         urban= `Manteniment de l'espai urbà`, 
         mobility= Mobilitat, 
         christmas=Nadal, 
         leisure= `Oci / lleure`, 
         security=`Prevenció i seguretat`,
         health=`Sanitat i salut pública`,
         transport=`Transports públics`, 
         urbanism=`Urbanisme, obres i habitatge`)
#get controls
library(haven)
controls <- read_dta("uar_quality_main.dta")%>%
  select(ANY_DATA_ALTA, MES_DATA_ALTA, CODI_DISTRICTE,districtname, population, density, districtname, date, 
         Income, participation, Educational_Attainment, votob, summer)%>%
  mutate(population_original=population, density_original=density, votos=votob/100)%>%
  select(-votob, -population, -density)
controls<-left_join(controls, density_district, by=c("ANY_DATA_ALTA"="Any", "CODI_DISTRICTE"="Codi_Districte"))
##overall

services<-left_join(services_wide, controls, by=c("CODI_DISTRICTE"="CODI_DISTRICTE", 
                                                  "ANY_DATA_ALTA"="ANY_DATA_ALTA",
                                                  "MES_DATA_ALTA"="MES_DATA_ALTA"))%>%
                      select(-Cultura, -`Informació, tràmits i atenció al ciutadà`, 
                             -`Manteniment de l'espai urbà`, -Mobilitat, -Nadal, 
                             -`Oci / lleure`, -`Prevenció i seguretat`, 
                             -Recollida,-Neteja, 
                             -`Sanitat i salut pública`, -`Transports públics`, 
                             -`Urbanisme, obres i habitatge`)

services$maintenance_pc<-(services$urban/services$pob)*10000
services$waste_pc<-(services$waste/services$pob)*10000
services$cleaning_pc<-(services$cleaning/services$pob)*10000
services$safety_pc<-(services$security/services$pob)*10000
services$health_pc<-(services$health/services$pob)*10000
services$mobility_pc<-(services$mobility/services$pob)*10000
services$urbanism_pc<-(services$urbanism/services$pob)*10000
services$transport_pc<-(services$transport/services$pob)*10000
services$christmas_pc<-(services$christmas/services$pob)*10000
services$info_pc<-(services$info/services$pob)*10000


library(foreign)
write.dta(services, "services.dta")


###encuesta

Mainsstenance of urban space	Waste collection and cleaning of urban space	Prevention and safety	Sanitation and public health	Mobility	Urbanism, works and housing	Christmas
decoration


library(purrr)
respuesta<-encuesta2019[["variables"]]%>%filter(ANY>=2014)%>%select(ANY, NOM_DISTRICTE,VAL_ASFALT,VAL_ESCOMBRARIES,VAL_NETEJA,VAL_SEGUR,
                              VAL_SOROLL,VAL_CIRCUL,VAL_URBA,VAL_FESTESPOP, LLAR_INGRESSOS_1A10, MUNI_VOTAR,MUNI_PARTIT,ESTUDIS_1A6
                              )%>%
modify_if(is.factor, as.character)
library(stringr)
encuesta<-function(x){
  ifelse(str_detect(x,"10 ="), "10",
         ifelse(str_detect(x,"0 ="), "0",
                ifelse(str_detect(x, "NO "), NA, x)))
}
respuesta$LLAR_INGRESSOS_1A10<-as.factor(respuesta$LLAR_INGRESSOS_1A10)
respuesta$MUNI_VOTAR<-as.factor(respuesta$MUNI_VOTAR)
respuesta$MUNI_PARTIT<-as.factor(respuesta$MUNI_PARTIT)

respuesta2<-respuesta%>%mutate(asphalt=as.numeric(encuesta(VAL_ASFALT)),
                               waste=as.numeric(encuesta(VAL_ESCOMBRARIES)),
                               cleaning=as.numeric(encuesta(VAL_NETEJA)),
                               safety=as.numeric(encuesta(VAL_SEGUR)),
                               sanitation=as.numeric(encuesta(VAL_SOROLL)),
                               circulation=as.numeric(encuesta(VAL_CIRCUL)),
                               urbanism=as.numeric(encuesta(VAL_URBA)),
                               christmas=as.numeric(encuesta(VAL_FESTESPOP)))
respuesta2$ESTUDIS_1A6<-as.factor(respuesta2$ESTUDIS_1A6)
respuesta2$eudational2<-recode(respuesta2$ESTUDIS_1A6,
                               "NO CONTESTA"="NA", 
                               "NO HA ACABAT ELS ESTUDIS OBLIGATORIS"="1",
                               "OBLIGATORIS"="2",
                               "SECUNDARIS GENERALS"="3",
                               "SECUNDARIS PROFESSIONALS"="4",
                               "UNIVERSITARIS"="5",
                               "POST UNIVERSITARIS"="6")
respuesta2$eudational2<-as.character(respuesta2$eudational2)
respuesta2$educational<-ifelse(respuesta2$ESTUDIS_1A6=="NO CONTESTA"|respuesta2$ESTUDIS_1A6=="NO HO SAP", NA, 
                          as.numeric(respuesta2$eudational2))
respuesta2$income2<-recode(respuesta2$LLAR_INGRESSOS_1A10,
       "MENYS DE 500 EUROS"="0",
       "DE 500 A 1.000 EUROS"="1",
       "DE 1.001 A 1.500 EUROS"="2",
       "DE 1.501 A 2.000 EUROS"="3", 
       "DE 2.001 A 2.500 EUROS"="4",
       "DE 2.501 A 3.000 EUROS"="5",
       "DE 3.001 A 5.000 EUROS"="6",
       "MÉS DE 5.000 EUROS" ="7",
       "DE 5.001 A 7.000 EUROS"="8",
       "DE 7.001 A 9.000 EUROS"="9",
       "MÉS DE 9.000 EUROS"="10",
       "NO CONTESTA" ="NA",
       "NO HO SAP"="NA")

       
respuesta2$income2<-as.character(respuesta2$income2)       
respuesta2$income<-ifelse(respuesta2$LLAR_INGRESSOS_1A10=="NO CONTESTA"|respuesta2$LLAR_INGRESSOS_1A10=="NO HO SAP", NA, 
                          as.numeric(respuesta2$income2))
respuesta2$voto<-ifelse(respuesta2$ANY==2014 & respuesta$MUNI_PARTIT=="CIU (XAVIER TRIAS)", 1,
                        ifelse(respuesta2$ANY!=2014 & respuesta$MUNI_PARTIT=="BARCELONA EN COMÚ (GUANYEM BARCELONA) (ADA COLAU)",1,
                               ifelse(respuesta$MUNI_PARTIT=="NO CONTESTA"|respuesta$MUNI_PARTIT=="NO HO SAP / NO HO RECORDA" ,NA, 0)))
       
       
respuesta2$participacion<-ifelse(respuesta2$MUNI_VOTAR=="VA VOTAR", 1, 
                                 ifelse(respuesta2$MUNI_VOTAR=="NO CONTESTA", NA, 0))

library(foreign)
write.dta(respuesta2, "encuesta.dta")                




means<-respuesta2%>%group_by(ANY, NOM_DISTRICTE2) %>%
  summarise(across(everything(), mean, na.rm = TRUE))
satisfaction<-respuesta%>%select(ANY, NOM_DISTRICTE, SATISF_RES_BCN_0A10)
names(means) <- sub(".*\\VAL_", "", names(means))

district_code$capital<-gsub("-", " - ", district_code$Nom_Districte)%>%
  toupper()%>%as.factor()
  
means<-left_join(means, district_code, by=c("NOM_DISTRICTE2"="capital"))
services_rent<-services%>%select(CODI_DISTRICTE, ANY_DATA_ALTA, Income, Educational_Attainment)%>%distinct()
means<-left_join(means, services_rent, by=c("Codi_Districte"="CODI_DISTRICTE", "ANY"="ANY_DATA_ALTA"))
                 
write.dta(means, "means.dta")                

                   