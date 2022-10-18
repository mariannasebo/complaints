###quejas+pop+surface

glimpse(merged6)
districts<-merged6%>%select(count, service, district, districtname, year,month,date, neigh,
                            pop, surface, density, Firm)%>%
  filter(service=="waste", year!=2013,)%>%distinct()%>%
  group_by(district, districtname, year,month,date, Firm)%>%
  summarise(complaints=sum(count), population=sum(pop), area=sum(surface))

districts<-districts%>%select(population, area, Firm, district, districtname, year, month, date)%>%distinct()
complaints_districts<-left_join(negbins3, districts, by=c("ANY_DATA_ALTA"="year", 
                                                          "MES_DATA_ALTA"="month", 
                                                          "CODI_DISTRICTE"="district"))

complaints_districts$complaints_pc<-complaints_districts$complaints/complaints_districts$population
complaints_districts$complaints_pc_10mill<-(complaints_districts$complaints/complaints_districts$population)*10000

##for quejas using negbins3

complaints_districts<-complaints_districts%>%
  filter(ANY_DATA_ALTA>2013)%>%ungroup()


###edu
glimpse(edu_district)
complaints_districts2<-left_join(complaints_districts, edu_district,
                                 by=c("ANY_DATA_ALTA"="Any",
                                      "CODI_DISTRICTE"="Codi_Districte"))
##EU
glimpse(EU_Esp_district)
complaints_districts3<-left_join(complaints_districts2, EU_Esp_district,
                                 by=c("ANY_DATA_ALTA"="Any",
                                      "CODI_DISTRICTE"="Codi_Districte"))
##rent1 
glimpse(rent_district)
complaints_districts4<-left_join(complaints_districts3, rent_district,
                                 by=c("ANY_DATA_ALTA"="Any",
                                      "CODI_DISTRICTE"="Codi_Districte"))
#rent2
library(readxl)
RDH <- read_excel("RDH.xlsx")
complaints_districts5<-left_join(complaints_districts4, RDH,
                                 by=c("ANY_DATA_ALTA"="year","CODI_DISTRICTE"="Codi_Districte"))


#participation

library(readxl)
turnout_district <- read_excel("turnout_district.xlsx", 
                               skip = 1)
turnout_district$turnout2011<-turnout_district$Votantes2011/turnout_district$Electores2011
turnout_district$turnout2015<-turnout_district$Votantes2015/turnout_district$Electores2015
turnout_district$turnout2019<-turnout_district$Votantes2019/turnout_district$Electores2019
##vote
glimpse(vote_uar)

complaints_districts6<-left_join(complaints_districts5, vote_uar,
                                 by=c("ANY_DATA_ALTA"="Any",
                                      "CODI_DISTRICTE"="Codi_Districte"))
complaints_districts6$votob<-ifelse(complaints_districts6$ANY_DATA_ALTA<2015, complaints_districts6$Vote2011,
                      ifelse(complaints_districts6$ANY_DATA_ALTA==2015 & complaints_districts6$MES_DATA_ALTA<=6,complaints_districts6$Vote2011,
                             ifelse(complaints_districts6$ANY_DATA_ALTA==2015 & complaints_districts6$MES_DATA_ALTA>=6,complaints_districts6$Vote2015,
                                    ifelse(complaints_districts6$ANY_DATA_ALTA>2015 & complaints_districts6$ANY_DATA_ALTA<2019,  complaints_districts6$Vote2015,
                                           ifelse(complaints_districts6$ANY_DATA_ALTA==2019 & complaints_districts6$MES_DATA_ALTA<=6,complaints_districts6$Vote2015,
                                                  ifelse(complaints_districts6$ANY_DATA_ALTA==2019 & complaints_districts6$MES_DATA_ALTA>=6,complaints_districts6$Vote2019, "0"
                                                  ))))))
complaints_districts6$votob<-as.numeric(complaints_districts6$votob)
complaints_districts6$RDH<-as.numeric(complaints_districts6$RDH)
complaints_districts6$density<-complaints_districts6$population/complaints_districts6$area
complaints_districts6$eu_esp<-complaints_districts6$total_euesp/complaints_districts6$total_pop
complaints_districts6$summer<-ifelse(complaints_districts6$MES_DATA_ALTA==8, 1, 0)
districtsfrom2014<-complaints_districts6
library(foreign)
write.dta(districtsfrom2014, "districts2014.dta")
districtsfrom2015<-complaints_districts6%>%filter(ANY_DATA_ALTA>2014)
write.dta(districtsfrom2015, "districts2015.dta")

#zones
zonesfrom2015<-districtsfrom2015%>%group_by(ANY_DATA_ALTA, MES_DATA_ALTA, Firm)%>%
  summarise(totalcomplaints=sum(complaints),
            poptotal=sum(total_pop))%>%ungroup()
zonecontrol<-recogida_final2quejas%>%select(rent3, date, Expenditure, ExPerCap,locfrent2, 
                                            CountPerCap, Count10mil, supStudies, 
                                            EU_Esp, votosB, Any, MES_DATA_ALTA, Provedor, Population2)
zonecontrol$Firm<-ifelse(zonecontrol$Provedor==1, "Cespa", 
                         ifelse(zonecontrol$Provedor==2, "CLD", 
                                ifelse(zonecontrol$Provedor==3, "FCC","Urbaser")))
zonecontrol$Provedor<-as.character(zonecontrol$Provedor)
zonesfrom2015_merged<-left_join(zonesfrom2015, zonecontrol, by=c("ANY_DATA_ALTA"="Any",
                                                                 "MES_DATA_ALTA"="MES_DATA_ALTA",
                                                                 "Firm"="Firm"))

zonesfrom2015_merged$complaintspercap2<-zonesfrom2015_merged$totalcomplaints/zonesfrom2015_merged$poptotal
zonesfrom2015_merged$Count10mil2<-(zonesfrom2015_merged$totalcomplaints/zonesfrom2015_merged$poptotal)*10000
library(foreign)
zonesfrom2015_merged$summer<-ifelse(zonesfrom2015_merged$MES_DATA_ALTA==8, 1, 0)

write.dta(zonesfrom2015_merged, "zonesfrom2015_merged.dta")
summary(recogida_final2quejas)


districts2014_with_renta <- read_dta("districts2014_with_renta.dta")
districts2014_with_renta$renta_final<-ifelse(districts2014_with_renta$ANY_DATA_ALTA==2014,(districts2014_with_renta$rent_district/100),districts2014_with_renta$RDH2)
districts2014_with_renta$renta_final2<-ifelse(districts2014_with_renta$ANY_DATA_ALTA==2014,districts2014_with_renta$rent_district,(districts2014_with_renta$RDH2*100))

library(readxl)

turnout_district <- read_excel("turnout_district.xlsx", 
                               skip = 1)
turnout_district$turnout2011<-turnout_district$Votantes2011/turnout_district$Electores2011
turnout_district$turnout2015<-turnout_district$Votantes2015/turnout_district$Electores2015
turnout_district$turnout2019<-turnout_district$Votantes2019/turnout_district$Electores2019

districts2014_with_renta<-left_join(districts2014_with_renta, turnout_district, by=c("CODI_DISTRICTE"="district_code"))


districts2014_with_renta$participation<-ifelse(districts2014_with_renta$ANY_DATA_ALTA<2015, districts2014_with_renta$turnout2011,
                                    ifelse(districts2014_with_renta$ANY_DATA_ALTA==2015 & districts2014_with_renta$MES_DATA_ALTA<=6,districts2014_with_renta$turnout2011,
                                           ifelse(districts2014_with_renta$ANY_DATA_ALTA==2015 & districts2014_with_renta$MES_DATA_ALTA>=6,districts2014_with_renta$turnout2015,
                                                  ifelse(districts2014_with_renta$ANY_DATA_ALTA>2015 & districts2014_with_renta$ANY_DATA_ALTA<2019,  districts2014_with_renta$turnout2015,
                                                         ifelse(districts2014_with_renta$ANY_DATA_ALTA==2019 & districts2014_with_renta$MES_DATA_ALTA<=6,districts2014_with_renta$turnout2015,
                                                                ifelse(districts2014_with_renta$ANY_DATA_ALTA==2019 & districts2014_with_renta$MES_DATA_ALTA>=6,districts2014_with_renta$turnout2019, "0"
                                                                ))))))
districts2014_with_renta$participation<-as.numeric(districts2014_with_renta$participation)

##change names of the variables 
#We have replaced ‘Superior Studies’ with ‘Educational Attainment’; 
#we have replaced ‘Rent’ with ‘Income’

districts2014_with_renta<-districts2014_with_renta%>%
  mutate(Income=renta_final,Educational_Attainment=sup_studies2, Year=ANY_DATA_ALTA,
         Income2=renta_final2)

#save in r

saveRDS(districts2014_with_renta, "uar_quality_main.rds")
library(foreign)
write.dta(districts2014_with_renta, "uar_quality_main.dta")

##info
glimpse(uar)
info<-uar%>%filter(AREA=="Informació i atenció ciutadana")%>%group_by(ANY_DATA_ALTA,Codi_Districte)%>%
  summarise(info=sum(n))%>%ungroup()%>%filter(ANY_DATA_ALTA>2013)


districts2014_with_renta<-left_join(districts2014_with_renta, info, by=c("CODI_DISTRICTE"="Codi_Districte", "Year"="ANY_DATA_ALTA"))
districts2014_with_renta$info_pc_10mill<-(districts2014_with_renta$info/districts2014_with_renta$population)*10000

saveRDS(districts2014_with_renta, "uar_quality_main.rds")
library(foreign)
write.dta(districts2014_with_renta, "uar_quality_main.dta")
quejas<-rbind(com2014, com2015, com2016, com2017, com2018,com2019, com2020)
quejas<-quejas%>%filter(ANY_DATA_ALTA!=2020)
quejas_other<-quejas
quejas_other<-quejas_other%>%select(TIPUS, AREA, ELEMENT, MES_DATA_ALTA, ANY_DATA_ALTA,
                                    CODI_DISTRICTE)%>%filter(TIPUS=="QUEIXA"| TIPUS=="INCIDENCIA")%>%
filter(AREA!="Recollida i neteja de l'espai urbà", 
       AREA!="Manteniment de l'espai urbà")
levels(as.factor(as.character(quejas_other$TIPUS)))
levels(as.factor(as.character(quejas_other$AREA)))
quejas_other$AREA2<-ifelse(quejas_other$AREA!="Serveis funeraris (cementiris i crematoris; tanatoris)", quejas_other$AREA,
                           "Serveis funeraris i cementiris")


levels(as.factor(as.character(quejas_other$AREA2)))
quejas_other<-quejas_other%>%filter(!is.na(CODI_DISTRICTE))
library(tidyr)
quejas_other_grouped_month<-quejas_other%>%group_by(AREA2, MES_DATA_ALTA, ANY_DATA_ALTA,
                                                    CODI_DISTRICTE)%>% tally()%>%
  ungroup()%>%
  complete(expand(quejas_other, CODI_DISTRICTE= 1:10,ANY_DATA_ALTA,MES_DATA_ALTA=1:12, AREA2),
           fill = list(n = 0))%>%filter(ANY_DATA_ALTA>2013)

quejas_other_grouped_month$AREA2<-as.factor(quejas_other_grouped_month$AREA2)

##### long to wide using spread() function of tidyr package

library(tidyr)

quejas_month_wide<-spread(quejas_other_grouped_month, AREA2, n)
quejas_month_wide
glimpse(quejas_month_wide)
summary(quejas_month_wide)

check_month<-quejas_month_wide
districts2014_with_renta<-left_join(districts2014_with_renta, check_month, by=c("CODI_DISTRICTE"="CODI_DISTRICTE", "Year"="ANY_DATA_ALTA", "MES_DATA_ALTA"="MES_DATA_ALTA" ))
districts2014_with_renta$cultura_pc<-(districts2014_with_renta$Cultura/districts2014_with_renta$population)*10000
districts2014_with_renta$info_pc<-(districts2014_with_renta$`Informació, tràmits i atenció al ciutadà`/districts2014_with_renta$population)*10000
districts2014_with_renta$mobility_pc<-(districts2014_with_renta$Mobilitat/districts2014_with_renta$population)*10000 
districts2014_with_renta$christmas_pc<-(districts2014_with_renta$Nadal/districts2014_with_renta$population)*10000
districts2014_with_renta$leisure_pc<-(districts2014_with_renta$`Oci / lleure` /districts2014_with_renta$population)*10000
districts2014_with_renta$security_pc<-(districts2014_with_renta$`Prevenció i seguretat`/districts2014_with_renta$population)*10000 
districts2014_with_renta$health_pc<-(districts2014_with_renta$`Sanitat i salut pública`/districts2014_with_renta$population)*10000
districts2014_with_renta$transport_pc<-(districts2014_with_renta$`Transports públics`/districts2014_with_renta$population)*10000
districts2014_with_renta$urbanism_pc<-(districts2014_with_renta$`Urbanisme, obres i habitatge`/districts2014_with_renta$population)*10000








saveRDS(districts2014_with_renta, "uar_quality_main.rds")
library(foreign)
write.dta(districts2014_with_renta, "uar_quality_main.dta")

#anual

check_year<-check_month%>%group_by(CODI_DISTRICTE,ANY_DATA_ALTA)%>%summarise(info=sum(`Informació, tràmits i atenció al ciutadà`),
                                                                             cultura=sum(Cultura), 
                                                                             mobility=sum(Mobilitat),
                                                                             christmas=sum(Nadal),
                                                                             leisure=sum(`Oci / lleure`),
                                                                             security=sum(`Prevenció i seguretat` ),
                                                                             health=sum(`Sanitat i salut pública`),
                                                                             transport=sum(`Transports públics`), 
                                                                             urbanism=sum(`Urbanisme, obres i habitatge` ))%>%ungroup()

year_control<-districts2014_with_renta%>%select(ANY_DATA_ALTA,CODI_DISTRICTE,total_pob,area,
                                                turnout2011,turnout2015,turnout2019, Income, Educational_Attainment,Vote2019,Vote2015,Vote2011, eu_esp)%>%distinct()
check_year<-left_join(check_year, year_control, by=c("CODI_DISTRICTE"="CODI_DISTRICTE", 
                                                     "ANY_DATA_ALTA"="ANY_DATA_ALTA"))
check_year$cultura_pc<-(check_year$cultura/check_year$total_pob)*10000
check_year$info_pc<-(check_year$info/check_year$total_pob)*10000
check_year$mobility_pc<-(check_year$mobility/check_year$total_pob)*10000 
check_year$christmas_pc<-(check_year$christmas/check_year$total_pob)*10000
check_year$leisure_pc<-(check_year$leisure/check_year$total_pob)*10000
check_year$security_pc<-(check_year$security/check_year$total_pob)*10000 
check_year$health_pc<-(check_year$health/check_year$total_pob)*10000
check_year$transport_pc<-(check_year$transport/check_year$total_pob)*10000
check_year$urbanism_pc<-(check_year$urbanism/check_year$total_pob)*10000
check_year$vote<-ifelse(check_year$ANY_DATA_ALTA<=2015, check_year$Vote2011,check_year$Vote2015)
check_year$participation<-ifelse(check_year$ANY_DATA_ALTA<=2015, check_year$turnout2011,check_year$turnout2015)
check_year<-check_year%>%select(-area)
check_year<-left_join(check_year,density_district, by=c("CODI_DISTRICTE"="Codi_Districte", 
                                                        "ANY_DATA_ALTA"="Any"))
check_year<-check_year%>%distinct()
saveRDS(check_year, "check_year.rds")
library(foreign)
write.dta(check_year, "check_year.dta")
