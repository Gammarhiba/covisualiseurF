#library(rsconnect) => use it to deploy the app
# then do this

#rsconnect::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>")
# you can find the account , token and secret in your shinyapps.io account

# go in the main directory that contains " app.R " then use "deployApp()"

library(shiny)
library(shinydashboard)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(maps)
library(shinyWidgets)
library(shinythemes)
library(lubridate)
library(fpp3)         # A newer tidy forecasting framework
library(tsibble)
library(scales)
library(gridExtra)
library(bslib)
library(plotly)


options(encoding = "UTF8")
# ajout du fichier qui contient les paramètres couleurs et tailles des graphiques
source("preamble.R",encoding = "UTF8")

current_date <- Sys.Date()

# le dataset qui contient la liste des noms de pays en français et anglais 
countries <- read.csv("data/sql-pays.csv",row.names = 1,encoding = "UTF8")
colnames(countries) <- c("Num", "Code", "AbFr", "Pays","Country")
countries <- countries %>% select("Pays","Country","Code")

# le dataset qui contient les noms des pays et leurs populations
countriesPop <- read_csv("data/population_by_country_2020.csv")
countriesPop <- rename(countriesPop, "Country" = "Country (or dependency)","Population" = "Population (2020)")
countriesPop <- countriesPop %>% select("Country","Population")

# le fichier qui contient les coordonnées des pays
coordonnees <- read_csv("data/world_country_and_usa_states_latitude_and_longitude_values.csv") %>% 
  select("latitude" ,"longitude","country") %>% rename("Country" = "country")

diffDI <- function(v){ 
  diff(c(0,v)) #difference d'elements de vecteur 
}

numberF <- function(n){
  number(n,scale = 1,big.mark =" ",decimal.mark=",",accuracy = 1)
}

percentF <- function(p){
  number(p,scale = 1,decimal.mark =",",accuracy = 0.1)
}


# Monde ----
##data github : deaths , cases ----

# la fonction qui traite le dataset des cas dans le monde
buildCases <- function(df,countriesPop,countries){
  df %>%
    pivot_longer(!c("Province/State","Country/Region","Lat","Long"),names_to = "Date",values_to = "Cases") %>%
    mutate(Date = mdy(Date)) %>%
    rename("Country" = "Country/Region","State" = "Province/State") %>%
    mutate(Country = str_replace(Country,"US","United States"),Country = str_replace(Country,"Cote d'Ivoire","Côte d'Ivoire"),
           Country = str_replace(Country,"Taiwan*","Taiwan"),Country = str_replace(Country,"Korea, South","South Korea"),
           Country = str_replace(Country,"Czechia","Czech Republic"),Country = str_replace(Country,"West Bank and Gaza","Palestine")) %>%
    left_join(countriesPop,by = "Country") %>%
    group_by(Country,State,Lat,Long) %>%
    mutate(newCases = diffDI(Cases) %>% pmax(0)) %>%
    group_by(Country,Date) %>%
    summarise(Cases = sum(Cases),newCases= sum(newCases),Population = mean(Population)) %>%
    left_join(countries,by = "Country") %>%
    left_join(coordonnees,by = "Country") %>%
    as_tsibble(index = Date, key = Country) %>%
    filter(!is.na(Country))
}


Cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>%
  buildCases(countriesPop,countries)

# la fonction qui traite le dataset des décès dans le monde
buildDeaths <- function(df,countriesPop,countries){
  df %>%
    pivot_longer(!c("Province/State","Country/Region","Lat","Long"),names_to = "Date",values_to = "Deaths") %>%
    mutate(Date = mdy(Date)) %>%
    rename("Country" = "Country/Region","State" = "Province/State") %>%
    mutate(Country = str_replace(Country,"US","United States"),Country = str_replace(Country,"Cote d'Ivoire","Côte d'Ivoire"),
           Country = str_replace(Country,"Taiwan*","Taiwan"),Country = str_replace(Country,"Korea, South","South Korea"),
           Country = str_replace(Country,"Czechia","Czech Republic"),Country = str_replace(Country,"West Bank and Gaza","Palestine")) %>%
    left_join(countriesPop,by = "Country") %>%
    group_by(Country,State,Lat,Long) %>%
    mutate(newDeaths = diffDI(Deaths) %>% pmax(0)) %>%
    group_by(Country,Date) %>%
    summarise(Deaths = sum(Deaths),newDeaths= sum(newDeaths),Population = mean(Population)) %>%
    left_join(countries,by = "Country") %>%
    left_join(coordonnees,by = "Country") %>%
    filter(!is.na(Code))
}


# casesP <- Cases %>%
#   as_tsibble(index = Date, key = c(Country,Pays,Code)) %>%
#   model(classical_decomposition(newCases, type = "multiplicative")) %>%
#   filter(Country == "France")

# Cases_comparaison <- 
#   components(casesP) %>% as_tsibble() %>% filter(Pays %in% c("France","Allemagne"))
# 
# comparaison_pays_plot(Cases_comparaison,min(Cases$Date))
# 
# casesPP <- Cases %>%
#   as_tsibble(index = Date, key = c(Country,Pays,Code)) %>% 
#   model(classical_decomposition(newCases, type = "multiplicative")) %>% 
#   filter(Country == "France")
# 
# PplotCases <- components(casesPP)  %>%
#   as_tsibble() %>%
#   autoplot(trend, colour =lkp_green,size = 1) +
#   scale_y_continuous(n.breaks = 5,labels = scales::number)+
#   scale_x_date(breaks = "3 month",minor_breaks = "1 month",date_labels = "%B %Y") + 
#   theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1)) +
#   labs(title = "Infections",
#        y = "Nombre des cas",
#        x = "Date",
#        subtitle = "On observe 3 vagues successives",
#        caption = "Données de l'université Jhon Hopkins")
# PplotCases
# 
# casesPPRecent <- Cases %>%
#   filter(Date>max(casesP$Date)+days(-30)) %>%
#   filter(Country == "France")
# 
# histogramCasMonde30 <- casesPPRecent %>% ggplot(aes(x=Date,weight = newCases)) +
#   geom_histogram(color="black",fill = lkp_green) +
#   scale_y_continuous(n.breaks = 5,labels = scales::number)+
#   scale_x_date(breaks = "1 week",minor_breaks = "1 day",date_labels = "%b %Y") + 
#   theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1)) +
#   labs(
#        y = "Infections des 30 derniers jours",
#        x = "Date")
# histogramCasMonde30
# 
# hp <- grid.arrange(PplotCases,histogramCasMonde30)
# print(PplotCases)
# deathsP <- Deaths %>% 
#   as_tsibble(index = Date, key = Country)%>%
#   pivot_longer(c("Deaths","newDeaths"),names_to = "typeDeaths",values_to = "number") %>%
#   model(classical_decomposition(number, type = "multiplicative")) %>% 
#   filter(Country == "France")

# plotCases <- components(casesP)  %>%
#   as_tsibble() %>%
#   autoplot(trend, colour =lkp_green,size = 1) +
#   facet_grid(typeCases~.,scales = "free") +
#   scale_y_continuous(n.breaks = 5,labels = scales::number)+
#   scale_x_date(breaks = "3 month",minor_breaks = "1 month",date_labels = "%B %Y") +
#   theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1)) +
#   labs(title = "Infections",
#        y = "Nombre des cas",
#        x = "Date",
#        subtitle = "On observe 3 vagues successives",
#        caption = "Données de l'université Jhon Hopkins")
# plotCases

# 
# plotDeaths <- components(deathsP)  %>%
#   as_tsibble() %>%
#   autoplot(trend, colour =lkp_blue,size = 1) +
#   facet_grid(typeDeaths~.,scales = "free") +
#   scale_y_continuous(n.breaks = 5,labels = scales::number)+
#   scale_x_date(breaks = "3 month",minor_breaks = "1 month",date_labels = "%B %Y") + 
#   theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1)) +
#   labs(title = "Décès",
#        y = "Nombre des décès",
#        x = "Date",
#        subtitle = "On observe 3 vagues successives",
#        caption = "Données de l'université Jhon Hopkins")
# plotDeaths


#graphics tsibble 

#### data regions france 

#load regions : dataset qui contient les noms des regions
regions <- read_csv("https://www.data.gouv.fr/fr/datasets/r/b6ea49c0-1e1b-4381-b92c-28c44088bf29") %>%
  rename("reg" = "code_region") %>%
  mutate(reg = as.factor(reg)) %>%
  select("reg","nom_region")

#load departement : dataset qui contient les noms des departements
departements <- read_csv("https://www.data.gouv.fr/fr/datasets/r/de7d0863-13e8-4010-9c75-487269f5d7ac") %>%
  rename("dep" = "code_departement") %>%
  mutate(dep = if_else(nchar(dep) == 1,str_c("0",dep),dep)) %>%
  mutate(dep = as.factor(dep)) %>%
  select("dep","nom_departement")

dataRegionsCas <- read_csv2("https://www.data.gouv.fr/fr/datasets/r/001aca18-df6a-45c8-89e6-f82d689e6c01")

dataRegionsCas <- left_join(regions, dataRegionsCas, by = "reg")

# coordonnes des regions et departements (longitude, latitude)
coordonneesRegDep <- read_delim("data/coordonneesRegDep.csv",delim=";") 

# coordonnées des departements
coordonneesDep <- coordonneesRegDep %>%
  filter(latitude != "NA" & longitude != "NA") %>%
  group_by(nom_departement) %>%
  summarise(latitude = min(latitude),longitude = min(longitude)) 

# les coordonnées des centres des departements
centresDep <- read_csv2("data/Centre_departement.csv") %>%
  rename("nom_departement" = "X2","longitude" = "Centre (« de gravité sur l'ellipsoïde »)","latitude" = "X5") 

centresDep <- centresDep[-1,] %>% select(nom_departement,longitude,latitude) %>%
  mutate(longitude1 = as.numeric(str_sub(longitude,1,1)) + as.numeric(str_sub(longitude,3,4))/60 +
           as.numeric(str_sub(longitude,6,7))/3600,
         latitude = as.numeric(str_sub(latitude,1,2)) + as.numeric(str_sub(latitude,4,5))/60 +
           as.numeric(str_sub(latitude,7,8))/3600) %>% 
  mutate(longitude = ifelse(str_sub(longitude,9,10) == " O",-longitude1,longitude1)) %>%
  select(nom_departement,longitude,latitude)

# les coordonnées des centres des regions
centresReg <- read_csv2("data/Centre_region.csv") %>%
  rename("nom_region" = "Régions métropolitaines","longitude" = "Centre (« de gravité sur l'ellipsoïde »)","latitude" = "X4")

centresReg <- centresReg[-1,] %>% select(nom_region,longitude,latitude) %>%
  mutate(longitude1 = as.numeric(str_sub(longitude,1,1)) + as.numeric(str_sub(longitude,3,4))/60 +
           as.numeric(str_sub(longitude,6,7))/3600,
         latitude = as.numeric(str_sub(latitude,1,2)) + as.numeric(str_sub(latitude,4,5))/60 +
           as.numeric(str_sub(latitude,7,8))/3600) %>% 
  mutate(longitude = ifelse(str_sub(longitude,9,10) == " O",-longitude1,longitude1)) %>%
  select(nom_region,longitude,latitude)

#la fonction qui traite le dataset des cas par departement
buildCasesDep <- function(df,departements,centresDep){
  df %>%
    filter(dep<=95) %>%
    mutate(dep = as.factor(dep),cl_age90 = factor(cl_age90,levels = c(09,19,29,39,49,59,69,79,89,90),
                                                  labels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79",
                                                             "80-89","supérier à 90"))) %>%
    mutate(jour = ymd(jour)) %>%
    left_join(departements,by = "dep") %>%
    left_join(centresDep,by = "nom_departement") %>%
    filter(cl_age90 != 0) %>%
    group_by(dep,jour,nom_departement) %>%
    summarise(P = sum(P),T = sum(T),longitude = mean(longitude),latitude = mean(latitude), pop = sum(pop))
}

dataDep <- read_delim("https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675",delim = ";")
  
# dataDepartementCasAges <- dataDep %>%
#   filter(dep<=95) %>%
#   mutate(dep = as.factor(dep),cl_age90 = fct_rev(factor(cl_age90,levels = c(09,19,29,39,49,59,69,79,89,90),
#                                                 labels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79",
#                                                            "80-89","supérier à 90")))) %>%
#   left_join(departements,by = "dep") %>%
#   left_join(centresDep,by = "nom_departement") %>%
#   filter(cl_age90 != 0)

dataDepartementCasTotal <- dataDep %>%
  buildCasesDep(departements,centresDep)

depPop <- dataDepartementCasTotal %>%
  group_by(dep) %>%
  summarise(pop=mean(pop))
  
# dataDepartementCasRecent <- dataDepartementCasAges %>%
#   filter(jour>max(dataDepartementCasTotal$jour)+days(-30)) %>%
#   filter(nom_departement == "Paris")
# 
# dataDepartementCasTotalRecent <- dataDepartementCasRecent %>%
#   group_by(dep,nom_departement,latitude,longitude) %>%
#   summarise(P = sum(P),T=sum(T),pop = mean(pop),tauxIncidence = sum(P)/mean(pop)*100000,
#             tauxPositivite = sum(P)/sum(T)*100000,tauxTests = sum(T)/mean(pop)*100000)
# 
# dataCasDepP <- dataDepartementCasTotal %>% 
#   as_tsibble(key = c(nom_departement,dep),index = jour) %>% 
#   model(classical_decomposition(P,type = "multiplicative")) %>%
#   filter(nom_departement == "Paris")
# 
# dataCasDepPAges <- dataDepartementCasAges %>% 
#   as_tsibble(key = c(nom_departement,dep,cl_age90),index = jour) %>% 
#   model(classical_decomposition(P,type = "multiplicative")) %>%
#   filter(nom_departement == "Paris")
# 
# dataTestDepP <- dataDepartementCasTotal %>% 
#   as_tsibble(key = c(nom_departement,dep),index = jour) %>% 
#   model(classical_decomposition(T,type = "multiplicative")) %>%
#   filter(nom_departement == "Paris")
# 
# dataTestDepPAges <- dataDepartementCasAges %>% 
#   as_tsibble(key = c(nom_departement,dep,cl_age90),index = jour) %>% 
#   model(classical_decomposition(T,type = "multiplicative")) %>%
#   filter(nom_departement == "Paris")
# 
# plotCasDep <- components(dataCasDepP) %>%
#   as_tsibble() %>%
#   autoplot(trend,colour = lkp_green,size = 1) + 
#   scale_y_continuous(n.breaks = 5,labels = scales::number)+
#   scale_x_date(breaks = "3 month",minor_breaks = "1 month",date_labels = "%b %Y") + 
#   theme(axis.text.x = element_text(vjust = 0.5,hjust = 0.5)) +
#   labs(title = "Infections",
#        y = "Infections",
#        x = "Date")
# plotCasDep
# 
# plotCasDepAges <- components(dataCasDepPAges) %>%
#   as_tsibble() %>%
#   ggplot(aes(x =jour, y=trend)) +
#   geom_line(aes(colour = factor(cl_age90)),size = 1) +
#   scale_y_continuous(n.breaks = 5,labels = scales::number)+
#   scale_x_date(breaks = "3 month",minor_breaks = "1 month",date_labels = "%b %Y") + 
#   theme(axis.text.x = element_text(vjust = 0.5,hjust = 0.5)) +
#   labs(title = "Infections",
#        y = "Infections",
#        x = "Date") + 
#   scale_color_brewer(palette="Spectral") +
#   theme(legend.position = "none")
# plotCasDepAges
# 
# plotTestDep <- components(dataTestDepP) %>%
#   as_tsibble() %>%
#   autoplot(trend,colour = lkp_blue,size = 1) 
# plotTestDep
# 
# plotTestDepAges <- components(dataTestDepPAges) %>%
#   as_tsibble() %>%
#   ggplot(aes(x =jour, y=trend)) +
#   geom_line(aes(colour = factor(cl_age90)),size = 1)
# plotTestDepAges
# 
# histogramCasDep30 <- dataDepartementCasRecent %>% ggplot(aes(x=jour,weight = P)) +
#   geom_histogram(color="black",fill = lkp_green) +
#   scale_y_continuous(n.breaks = 5,labels = scales::number)+
#   scale_x_date(breaks = "1 week",minor_breaks = "1 day",date_labels = "%b %Y") + 
#   theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1)) +
#   labs(title = "Infections des 30 derniers jours",
#        y = "Infections",
#        x = "Date")
# histogramCasDep30
# 
# histogramCasDep30ages <- dataDepartementCasRecent %>% ggplot(aes(x=jour,weight = P,fill=cl_age90)) +
#   geom_histogram(color="black") 
# histogramCasDep30ages

# coordonnées des regions 
coordonneesReg <- coordonneesRegDep %>% group_by(nom_region) %>% summarise(latitude = mean(latitude),longitude = mean(longitude)) %>% 
  rename("maille_nom" = "nom_region") 

#la fonction qui traite le dataset des cas par region
buildCasesReg <- function(df,regions,centresReg){
  df %>%
    mutate(reg = as.factor(reg)) %>%
    mutate(jour = ymd(jour)) %>%
    left_join(regions,by = "reg") %>%
    left_join(centresReg,by = "nom_region") %>%
    filter(cl_age90 != 0) %>%
    group_by(reg,jour,nom_region) %>%
    summarise(P = sum(P),T = sum(T),longitude = mean(longitude),latitude = mean(latitude), pop = sum(pop),P_f = sum(P_f),P_h = sum(P_h))
}

dataReg <- read_delim("https://www.data.gouv.fr/fr/datasets/r/001aca18-df6a-45c8-89e6-f82d689e6c01",delim = ";")

# dataRegionCasAges <- dataReg %>%
#   mutate(reg = as.factor(reg),cl_age90 = fct_rev(factor(cl_age90))) %>%
#   left_join(regions,by = "reg") %>%
#   left_join(centresReg,by = "nom_region") %>%
#   filter(cl_age90 != 0)
# 
# dataRegionCasSexe <- dataReg %>%
#   mutate(reg = as.factor(reg)) %>%
#   left_join(regions,by = "reg") %>%
#   left_join(centresReg,by = "nom_region") %>%
#   group_by(reg,nom_region,jour) %>%
#   summarise(latitude = mean(latitude),longitude = sum(longitude),P = sum(P),T=sum(T),
#             P_h = sum(P_h),P_f = sum(P_f),T_h = sum(T_h),T_f = sum(T_f),pop = mean(pop))
# 
# dataRegionCasSexeP <- dataRegionCasSexe %>%
#   select(!c(P,T,T_h,T_f)) %>%
#   pivot_longer(c(P_h,P_f),names_to = "sexe",values_to = "P") %>%
#   mutate(sexe = str_replace(sexe,"P_h","1"),sexe = str_replace(sexe,"P_f","2") ) %>%
#   mutate(sexe = factor(sexe,levels = c(1,2),labels = c("Hommes","Femmes"))) 
# 
# dataRegionCasSexeT <- dataRegionCasSexe %>%
#   left_join(centresReg,by = "nom_region") %>%
#   select(!c(P,T,P_h,P_f)) %>%
#   pivot_longer(c(T_h,T_f),names_to = "sexe",values_to = "T") %>%
#   mutate(sexe = str_replace(sexe,"T_h","1"),sexe = str_replace(sexe,"T_f","2") ) %>%
#   mutate(sexe = factor(sexe,levels = c(1,2),labels = c("Hommes","Femmes")))

dataRegionCasTotal <- dataReg %>%
  buildCasesReg(regions,centresReg)

regPop <- dataRegionCasTotal %>%
  group_by(reg) %>%
  summarise(pop=mean(pop))


# dataRegionCasTotalFinal <-
#   dataRegionCasTotal %>%
#     group_by(nom_region,reg,latitude,longitude) %>%
#     summarise(P = sum(P), T = sum(T), pop = mean(pop),tauxIncidence = sum(P)/mean(pop)*100000,
#               tauxPositivite = sum(P)/sum(T)*100000,tauxTests = sum(T)/mean(pop)*100000)


# chiffresCles <- read_csv("https://www.data.gouv.fr/fr/datasets/r/0b66ca39-1623-4d9c-83ad-5434b7f9e2a4")
# 
# chiffresClesRegions <- chiffresCles %>% filter(granularite == "region") %>% left_join(coordonneesReg,by = "maille_nom")
# chiffresClesDeps <- chiffresCles %>% filter(granularite == "departement")
# 
# chiffresClesRegionsRecent <- chiffresClesRegions %>% filter(date == max(chiffresClesRegions$date))


#la fonction qui traite le dataset des hosps par departement
buildHospDep <- function(df,departements,centresDep){
  df %>%
    filter(dep<=95) %>%
    left_join(depPop,by="dep") %>%
    filter(sexe != 0) %>%
    mutate(dep = as.factor(dep),sexe = factor(sexe,levels = c(1,2),labels = c("Hommes","Femmes")),dc = as.integer(dc)) %>%
    mutate(jour = ymd(jour)) %>%
    left_join(departements,by = "dep") %>%
    left_join(centresDep,by = "nom_departement") %>%
    group_by(nom_departement,dep,sexe,longitude,latitude) %>%
    mutate(nv_dc = diffDI(dc),nv_hosp = diffDI(hosp) %>% pmax(0),nv_rea = diffDI(rea) %>% pmax(0)) %>%
    group_by(dep,jour,nom_departement) %>%
    summarise(hosp = sum(hosp),rea = sum(rea),longitude = mean(longitude),
              latitude = mean(latitude), rad = sum(rad),pop=mean(pop),nv_dc = sum(nv_dc),dc= sum(dc),nv_rea = sum(nv_rea),nv_hosp = sum(nv_hosp))
}


dataDepHosp <- read_delim("https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7",delim = ";")
dataDepHospTotal <- dataDepHosp %>% buildHospDep(departements,centresDep)
  
# p <- dataDepHospTotal %>% filter(nom_departement == "Paris") %>%
#   ggplot(aes(x =jour, y=nv_dc)) +
#   geom_line(color = "black")
# p
# 
# dataDepartementHospSexe <- dataDepHosp %>%
#   left_join(depPop,by="dep") %>%
#   filter(dep<=95) %>%
#   mutate(dep = as.factor(dep),sexe = factor(sexe,levels = c(1,2),labels = c("Hommes","Femmes")),dc = as.integer(dc)) %>%
#   left_join(departements,by = "dep") %>%
#   left_join(centresDep,by = "nom_departement") %>%
#   filter(sexe != 0) %>%
#   group_by(nom_departement,dep,sexe,longitude,latitude) %>%
#   mutate(nv_dc = diffDI(dc),nv_hosp = diffDI(hosp) %>% pmax(0),nv_rea = diffDI(rea) %>% pmax(0))
# 
# dataDepartementHospRecent <- dataDepartementHospSexe %>%
#   filter(jour>max(dataDepHospTotal$jour)+days(-30)) %>%
#   filter(nom_departement == "Paris")
# 
# dataDepartementHospTotalRecent <- dataDepartementHospRecent %>%
#   group_by(dep,nom_departement,latitude,longitude) %>%
#   summarise(hosp = sum(hosp),rea = sum(rea),rad = sum(rad),nv_dc = sum(nv_dc),pop=mean(pop)
#             ,tauxHosps = sum(hosp)/mean(pop)*100000,tauxDeces = sum(nv_dc)/mean(pop)*100000,tauxRea = sum(nv_rea)/mean(pop)*100000,nv_hosp = sum(nv_hosp),nv_rea = sum(nv_rea))
# 
# dataHospDepP <- dataDepHospTotal %>% 
#   as_tsibble(key = c(nom_departement,dep),index = jour) %>% 
#   model(classical_decomposition(hosp,type = "multiplicative")) %>%
#   filter(nom_departement == "Paris")
# 
# dataDecesDepP <- dataDepHospTotal %>% 
#   as_tsibble(key = c(nom_departement,dep),index = jour) %>% 
#   model(classical_decomposition(nv_dc,type = "multiplicative")) %>%
#   filter(nom_departement == "Paris")
# 
# dataHospDepPSexe <- dataDepartementHospSexe %>% 
#   as_tsibble(key = c(nom_departement,dep,sexe),index = jour) %>% 
#   model(classical_decomposition(hosp,type = "multiplicative")) %>%
#   filter(nom_departement == "Paris")
# 
# dataReaDepP <- dataDepHospTotal %>% 
#   as_tsibble(key = c(nom_departement,dep),index = jour) %>% 
#   model(classical_decomposition(rea,type = "multiplicative")) %>%
#   filter(nom_departement == "Paris")
# 
# dataReaDepPSexe <- dataDepartementHospSexe %>% 
#   as_tsibble(key = c(nom_departement,dep,sexe),index = jour) %>% 
#   model(classical_decomposition(rea,type = "multiplicative")) %>%
#   filter(nom_departement == "Paris")
# 
# dataDecesaDepPSexe <- dataDepartementHospSexe %>% 
#   as_tsibble(key = c(nom_departement,dep,sexe),index = jour) %>% 
#   model(classical_decomposition(nv_dc,type = "multiplicative")) %>%
#   filter(nom_departement == "Paris")
# 
# plotHospDep <- components(dataHospDepP) %>%
#   as_tsibble() %>%
#   autoplot(trend,colour = lkp_green,size = 1) 
# plotHospDep
# 
# plotHospDepSexe <- components(dataHospDepPSexe) %>%
#   as_tsibble() %>%
#   ggplot(aes(x =jour, y=trend)) +
#   geom_line(aes(colour = factor(sexe)),size = 1)
# plotHospDepSexe
# 
# plotReaDep <- components(dataReaDepP) %>%
#   as_tsibble() %>%
#   autoplot(trend,colour = lkp_blue,size = 1) 
# plotReaDep
# 
# plotReaDepSexe <- components(dataReaDepPSexe) %>%
#   as_tsibble() %>%
#   ggplot(aes(x =jour, y=trend)) +
#   geom_line(aes(colour = factor(sexe)),size = 1)
# plotReaDepSexe
# 
# histogramHospDep30 <- dataDepartementHospRecent %>% ggplot(aes(x=jour,weight = hosp)) +
#   geom_histogram(color="black",fill = lkp_green) +
#   scale_y_continuous(n.breaks = 5,labels = scales::number)+
#   scale_x_date(breaks = "1 week",minor_breaks = "1 day",date_labels = "%b %Y") + 
#   theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1)) +
#   labs(title = "Hospitalisations des 30 derniers jours",
#        y = "Hospitalisations",
#        x = "Date")
# histogramHospDep30
# 
# histogramHospDep30sexe <- dataDepartementHospRecent %>% ggplot(aes(x=jour,weight = hosp,fill=sexe)) +
#   geom_histogram(color="black") 
# histogramHospDep30sexe


#la fonction qui traite le hosps des cas par region

buildHospReg <- function(df,regions,centresReg){
  df %>%
    filter(cl_age90 != 0) %>%
    mutate(reg = as.factor(reg),cl_age90 =  fct_rev(factor(cl_age90,levels = c(09,19,29,39,49,59,69,79,89,90),
                                                   labels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79",
                                                              "80-89","supérier à 90")))) %>%
    mutate(jour = ymd(jour)) %>%
    left_join(regions,by = "reg") %>%
    left_join(regPop,by="reg") %>%
    left_join(centresReg,by = "nom_region") %>%
    group_by(nom_region,reg,cl_age90,longitude,latitude) %>%
    mutate(nv_dc = diffDI(dc),nv_hosp = diffDI(hosp) %>% pmax(0),nv_rea = diffDI(rea) %>% pmax(0)) %>%
    group_by(reg,jour,nom_region) %>%
    summarise(hosp = sum(hosp),rea = sum(rea),longitude = mean(longitude),
              latitude = mean(latitude), rad = sum(rad),pop=mean(pop),nv_dc = sum(nv_dc),dc= sum(dc),nv_rea = sum(nv_rea),nv_hosp = sum(nv_hosp))
}


# dataRegHosp <- read_delim("https://www.data.gouv.fr/fr/datasets/r/08c18e08-6780-452d-9b8c-ae244ad529b3",delim = ";")
# dataRegHospTotal <- dataRegHosp %>%
#     buildHospReg(regions,centresReg)
