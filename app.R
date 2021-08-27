# import du fichier data-V1 qui contient les traitements des datasets statiques
# ainsi que les imports des bibliothèques utilisées

source("data-V1.R",encoding = "UTF8")
Sys.setlocale("LC_CTYPE","fr_FR.UTF-8")
options(scipen=999)
# augmentation de temps de timeout en cas de problème d'import des données
options(timeout= 400000000)

# La partie UI du code, qui contient les éléments et parties de la page web
ui <- bootstrapPage(includeCSS("styles.css"),
  navbarPage(
    # mise de thème par défaut "journal" avec un changement des couleurs et polices utilisés
    theme = bslib::bs_theme(bootswatch = "journal",bg = "white", 
                                     fg = lkp_blue, 
                                     primary = lkp_green,
                                     secondary = lkp_blue,
                                     base_font = "serif",
                                     success = lkp_blue,
                                     base_size = 8,
                                     info = lkp_blue,
                                     warning = lkp_blue,
                                     danger = lkp_blue
                            )   %>%
               bslib::bs_add_variables(
                 "font-family-base" = "serif",
                 "font-size-base" = "1.2rem",
                 "font-style-base" = "bold")
    , collapsible = TRUE,
              # ajout de logo linkpact
             HTML('<img src = "linkpact-logo-p.png"><a style="text-decoration:none;cursor:default;font-color:"008F5A"; font-style: "serif" class="active" href="#" color = "008F5A">&nbsp;&nbsp;&nbsp;Covisualiseur </a>'), id="nav",
             windowTitle = "LinkPact Covisualiseur",
             
            # l'onglet monde
             tabPanel("Monde ",
                      fluidRow(class="outer",
                          # la colonne qui contient la carte du monde
                          column(12,leafletOutput("map",height = "100%",width = "98%"),
                          #les deux panels de chiffres clés
                          absolutePanel(id = "chiffresCle", class = "panel panel-default",
                                        top = 470, left = 5, width = 130,height = 210, fixed=TRUE,
                                        draggable = TRUE,
                                        em(textOutput("jours"), align = "center"),
                                        h6(textOutput("monde_cases_count"), align = "center",style="color:darkorange"),
                                        h6(textOutput("monde_deaths_count"), align = "center",style="color:black")
                                        ),
                          absolutePanel(id = "chiffresCle", class = "panel panel-default",
                                        top = 250, left = 5, width = 130,height = 210, fixed=TRUE,
                                        draggable = TRUE,
                                        em(textOutput("debut"), align = "center"),
                                        h6(textOutput("mondeD_cases_count"), align = "center",style="color:darkorange"),
                                        h6(textOutput("mondeD_deaths_count"), align = "center",style="color:black"))))
                      
             ),
            # onglet france par région
             tabPanel("France par région ",
                      fluidPage(
                          tabBox(
                            width = "95%",height = "98%",
                            tabPanel("Infections",
                                     fluidRow(
                                    # colonne qui contient la carte de france par région ( cas)
                                     column(8,leafletOutput("mapRegionC",height = 600,width = "98%")),
                                     # le panel droite qui contient les graphiques
                                     column(6,absolutePanel(id = "controls", class = "panel panel-default",
                                                   top = "13%", left = "64%", width = "50%",height = "90%", fixed=TRUE,
                                                   draggable = FALSE,
                                                   # courbe cas région
                                                   plotOutput("graphCR",height = "40%",width = "70%"),
                                                   # histogram cas région
                                                   plotOutput("histogramCR",height = "45%",width = "70%"),
                                                   # checkbox age ou sexe pour cas 
                                                   checkboxGroupInput("ageSexeC", label = "Afficher par :",choices = c("Classe d'âge","Sexe"),selected = NULL)))
                                     )),
                            # de la même manière sont organisés les cartes et graphiques dans toute la partie france par région
                            tabPanel("Tests",
                                     fluidRow(
                                     column(8,leafletOutput("mapRegionT",height = 600,width = "98%")),
                                     column(6,absolutePanel(id = "controls", class = "panel panel-default",
                                                            top = "13%", left = "64%", width = "50%",height = "90%", fixed=TRUE,
                                                   draggable = FALSE,
                                                   plotOutput("graphTR",height = "40%",width = "70%"),
                                                   plotOutput("histogramTR",height = "45%",width = "70%"),
                                                   checkboxGroupInput("ageSexeT", label = "Afficher par :",choices = c("Classe d'âge","Sexe"),selected = NULL))
                                     ))),
                            tabPanel("Hospitalisations",
                                     fluidRow(
                                     column(8,leafletOutput("mapRegionH",height = 600,width = "98%")),
                                     column(6,absolutePanel(id = "controls", class = "panel panel-default",
                                                            top = "13%", left = "64%", width = "50%",height = "90%", fixed=TRUE,
                                                   draggable = FALSE,
                                                   plotOutput("graphHR",height = "40%",width = "70%"),
                                                   plotOutput("histogramHR",height = "45%",width = "70%"),
                                                   checkboxInput("ageRegH", "Afficher par classe d'âge", value = FALSE,width = "100%"))))),
                            tabPanel("Réanimations",
                                     fluidRow(
                                     column(8,leafletOutput("mapRegionR",height = 600,width = "98%")),
                                     column(6,absolutePanel(id = "controls", class = "panel panel-default",
                                                            top = "13%", left = "64%", width = "50%",height = "90%", fixed=TRUE,
                                                   draggable = FALSE,
                                                   plotOutput("graphRR",height = "40%",width = "70%"),
                                                   plotOutput("histogramRR",height = "45%",width = "70%"),
                                                   checkboxInput("ageRegR", "Afficher par classe d'âge", value = FALSE,width = "100%"))))),
                            tabPanel("Décès à l'hopital",
                                     fluidRow(
                                     column(8,leafletOutput("mapRegionD",height = 600,width = "98%")),
                                     column(6,absolutePanel(id = "controls", class = "panel panel-default",
                                                            top = "13%", left = "64%", width = "50%",height = "90%", fixed=TRUE,
                                                   draggable = FALSE,
                                                   plotOutput("graphDR",height = "40%",width = "70%"),
                                                   plotOutput("histogramDR",height = "45%",width = "70%"),
                                                   checkboxInput("ageRegD", "Afficher par classe d'âge", value = FALSE,width = "100%")))))
                          ))
                      ),
             
            # onglet france par département
             tabPanel("France par département ",
                      fluidPage(
                        tabBox(
                          width = "95%",height = "100%",
                        tabPanel("Infections",
                                 fluidRow(
                                   # colonne qui contient carte de france par département ( cas )
                                 column(8,leafletOutput("mapDepartementC",height = 600,width = "98%")),
                                 # le panel droite qui contient les graphiques
                                 column(6,absolutePanel(id = "controls", class = "panel panel-default",
                                                        top = "13%", left = "64%", width = "50%",height = "90%", fixed=TRUE,
                                               draggable = FALSE,
                                               # courbe département cas
                                               plotOutput("graphCD",height = "40%",width = "70%"),
                                               # histogramme département cas
                                               plotOutput("histogramCD",height = "45%",width = "70%"),
                                               #checkbox age pour cas
                                               checkboxInput("ageDepC", "Afficher par classe d'âge", value = FALSE,width = "100%"))))
                        ),
                        # de la même manière sont organisés les cartes et graphiques dans toute la partie france par département
                        tabPanel("Tests",
                                 fluidRow(
                                 column(8,leafletOutput("mapDepartementT",height = 600,width = "98%")),
                                 column(6,absolutePanel(id = "controls", class = "panel panel-default",
                                                        top = "13%", left = "64%", width = "50%",height = "90%", fixed=TRUE,
                                               draggable = FALSE,
                                               plotOutput("graphTD",height = "40%",width = "70%"),
                                               plotOutput("histogramTD",height = "45%",width = "70%"),
                                               checkboxInput("ageDepT", "Afficher par classe d'âge", value = FALSE,width = "100%"))))),
                        tabPanel("Hospitalisations",
                                 fluidRow(
                                 column(8,leafletOutput("mapDepartementH",height = 600,width = "98%")),
                                 column(6,absolutePanel(id = "controls", class = "panel panel-default",
                                                        top = "13%", left = "64%", width = "50%",height = "90%", fixed=TRUE,
                                               draggable = FALSE,
                                               plotOutput("graphHD",height = "40%",width = "70%"),
                                               plotOutput("histogramHD",height = "45%",width = "70%"),
                                               checkboxInput("sexeDepH", "Afficher par sexe", value = FALSE,width = "100%"))))),
                        tabPanel("Réanimations",
                                 fluidRow(
                                 column(8,leafletOutput("mapDepartementR",height = 600,width = "98%")),
                                 column(6,absolutePanel(id = "controls", class = "panel panel-default",
                                                        top = "13%", left = "64%", width = "50%",height = "90%", fixed=TRUE,
                                               draggable = FALSE,
                                               plotOutput("graphRD",height = "40%",width = "70%"),
                                               plotOutput("histogramRD",height = "45%",width = "70%"),
                                               checkboxInput("sexeDepR", "Afficher par sexe", value = FALSE,width = "100%"))))),
                        tabPanel("Décès à l'hopital",
                                 fluidRow(
                                 column(8,leafletOutput("mapDepartementD",height = 600,width = "98%")),
                                 column(6,absolutePanel(id = "controls", class = "panel panel-default",
                                                        top = "13%", left = "64%", width = "50%",height = "90%", fixed=TRUE,
                                               draggable = FALSE,
                                               plotOutput("graphDD",height = "40%",width = "70%"),
                                               plotOutput("histogramDD",height = "45%",width = "70%"),
                                               checkboxInput("sexeDepD", "Afficher par sexe", value = FALSE,width = "100%")))))
                      ))
  ),
  
  # l'onglet comparaison
  tabPanel("Comparaison ",
            
           fluidPage(
             tabBox(
               width = "95%",height = "100%",
               # l'onglet pays
               tabPanel("Pays",
                        fluidRow(
                          # le graphique des comparaison des pays
                          column(8,plotlyOutput("comparaison_pays")),
                          # panel de control
                          column(6,absolutePanel(id = "controls", class = "panel panel-default",
                                                 top = "20%", left = "75%", width = "35%",height = "90%", fixed=TRUE,
                                                 draggable = FALSE,
                                                # choix de l'indicateur
                                                 pickerInput("outcome_select", "Indicateur:",   
                                                             choices = c("Infections", "Décès"), 
                                                             selected = c("Infections"),
                                                             multiple = FALSE),
                                                # choix de pays
                                                 pickerInput("country_select", "Pays",   
                                                             choices = as.character(unique(Cases$Pays)), 
                                                             options = list(`actions-box` = TRUE, `none-selected-text` = "Choisir un ou plusieurs pays!"),
                                                             selected = "France",
                                                             multiple = TRUE),
                                                # choix de date
                                                 sliderInput("minimum_date",
                                                             "A partir de :",
                                                             min = as.Date(min(Cases$Date)),
                                                             max = as.Date(current_date),
                                                             value = as.Date(min(Cases$Date)),
                                                             timeFormat="%b %Y"),
                                                 ))))
               ,
# de la même manière sont organisés les différentes parties de l'onglet comparaison, on change juste pays par région ou département
               tabPanel("Régions",
                        fluidRow(
                          column(8,plotlyOutput("comparaison_region")),
                          column(6,absolutePanel(id = "controls", class = "panel panel-default",
                                                 top = "20%", left = "75%", width = "35%",height = "90%", fixed=TRUE,
                                                 draggable = FALSE,
                                                 
                                                 pickerInput("outcome_selectR", "Indicateur:",   
                                                             choices = c("Infections","Tests","Hospitalisations","Réanimations","Décès"), 
                                                             selected = c("Infections"),
                                                             multiple = FALSE),
                                                 pickerInput("region_select", "Région",   
                                                             choices = as.character(regions$nom_region), 
                                                             options = list(`actions-box` = TRUE, `none-selected-text` = "Choisir une ou plusieurs régions!"),
                                                             selected = "Île-de-France",
                                                             multiple = TRUE),
                                                 sliderInput("minimum_dateR",
                                                             "A partir de :",
                                                             min = as.Date(min(Cases$Date),"%Y-%m-%d"),
                                                             max = as.Date(current_date,"%Y-%m-%d"),
                                                             value = as.Date(min(Cases$Date),"%Y-%m-%d"),
                                                             timeFormat="%b %Y"),
                          )))),
               
               tabPanel("Départements",
                        fluidRow(
                          column(8,plotlyOutput("comparaison_dep")),
                          column(6,absolutePanel(id = "controls", class = "panel panel-default",
                                                 top = "20%", left = "75%", width = "35%",height = "90%", fixed=TRUE,
                                                 draggable = FALSE,
                                                 
                                                 pickerInput("outcome_selectD", "Indicateur:",   
                                                             choices = c("Infections","Tests","Hospitalisations","Réanimations","Décès"), 
                                                             selected = c("Infections"),
                                                             multiple = FALSE),
                                                 pickerInput("departement_select", "Département",   
                                                             choices = as.character(departements$nom_departement), 
                                                             options = list(`actions-box` = TRUE, `none-selected-text` = "Choisir un ou plusieurs pays!"),
                                                             selected = "Paris",
                                                             multiple = TRUE),
                                                 sliderInput("minimum_dateD",
                                                             "A partir de :",
                                                             min = as.Date(min(Cases$Date),"%Y-%m-%d"),
                                                             max = as.Date(current_date,"%Y-%m-%d"),
                                                             value = as.Date(min(Cases$Date),"%Y-%m-%d"),
                                                             timeFormat="%b %Y"),
                          ))))
             ))
  )
))

# la partie server, c'est la partie reactive qui contient les traitements des données reactives, 
# toute la partie cartes et leaflets, et la création des graphiques qui dépendent de la carte ( reactives)

server <- function(input, output, session) {
  
  ## data monde ---
      # l'import des données reactives cas et deces dans le monde
      # l'import est reactif pour permettre la mise à jour des nouveaux données
  cases <- reactiveFileReader(7200000,session,"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",readFunc =  read_csv) 
  
  deaths <- reactiveFileReader(7200000,session,"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",readFunc =  read_csv) 
  
  # application des traitements nécessaires sur les données cases à l'aide de les fonctions buildCases et buildDeaths
  Cases <- reactive({
    cases() %>%
      buildCases(countriesPop,countries)
  })
  
  Deaths <- reactive({
    deaths() %>%
      buildDeaths(countriesPop,countries) 
  })

  # création de datasets reactives à utiliser dans a cartes du monde [ CasesL, DeathsL ]
  CasesL <- reactive({
    date_diff_monde <- as.integer(gsub("[a-zA-Z,0H,0M,0S]", "", as.character(days(max(Cases()$Date) - min(Cases()$Date)))))
    nb_months_monde <- date_diff_monde/30
    Cases() %>% as_tibble() %>%
      filter(Date> max(Cases()$Date)+(days(-30))) %>% 
      group_by(Country,latitude,longitude,Pays) %>%
      summarise(Cases = max(Cases),newCases= sum(newCases),tauxInfection =sum(newCases)/mean(Population)*100000,
                cases100milles = (max(Cases)/mean(Population)*100000)/nb_months_monde,Population = mean(Population)) %>%
      filter(!is.na(tauxInfection))
  })
  
  DeathsL <- reactive({
    date_diff_monde <- as.integer(gsub("[a-zA-Z,0H,0M,0S]", "", as.character(days(max(Deaths()$Date) - min(Deaths()$Date)))))
    nb_months_monde <- date_diff_monde/30
    
    Deaths() %>% as_tibble() %>%
      mutate(day = as.numeric(strftime(Date, format = "%j")),year = as.numeric(strftime(Date, format = "%y"))) %>%
      filter((year == max(year))) %>% 
      filter(day %in% seq(from = max(day),to = max(day)-30)) %>%
      group_by(Country,latitude,longitude) %>%
      summarise(Deaths = max(Deaths),newDeaths= sum(newDeaths),tauxDeces =sum(newDeaths)/mean(Population)*100000,
                deaths100milles = (max(Deaths)/mean(Population)*100000)/nb_months_monde,Pays = Pays,Code = Code) %>%
      filter(!is.na(tauxDeces))
  })
  
  # création des datasets reactives à utiliser dans les traçages des graphiques pour le monde
  # en appliquant le modèle de décomposition [casesP , deathsP]
  casesP <- reactive({
    Cases() %>% 
      as_tsibble(index = Date, key = c(Country,Pays,Code)) %>% 
      model(classical_decomposition(newCases, type = "multiplicative"))
  })

  deathsP <- reactive({
    Deaths() %>% 
      as_tsibble(index = Date, key = c(Country,Pays,Code))%>%
      model(classical_decomposition(newDeaths, type = "multiplicative"))
  })
  
  # création des datasets reactives à utiliser pour traçage de graphique de comparaison des pays 
  Cases_comparaison <- reactive({
    components(casesP() %>% filter(Pays %in% input$country_select)) %>% as_tsibble()
  })
  
  Deaths_comparaison <- reactive({
    components(deathsP()) %>% as_tsibble() %>% filter(Pays %in% input$country_select)
  })
  
  # datasets pour affichage des chiffres clés sur la carte
  casesPRecent <- reactive({
    Cases() %>%
      filter(Date>max(Cases()$Date)+days(-30))
  })
  deathsPRecent <- reactive({
    Deaths() %>%
      filter(Date>max(Deaths()$Date)+days(-30))
  })
  #----------------------------------------------------------------------------------
  #création des fonctions qui permet de tracer les graphiques de comparaison de pays et regions 
  # elles prennent en paramètres le dataset utilisé et la date de début de visualisation
  
  #graphique comparaisons infections pays
  comparaison_pays_plot <- function(df_cases,start_date){
    g <-  df_cases %>%
    ggplot(aes(x =Date, y=trend)) +
      scale_y_continuous(n.breaks = 5,labels = scales::number)+
      scale_x_date(breaks = "3 month",minor_breaks = "1 month",date_labels = "%b %Y") +
      xlim(c(start_date,(current_date+5))) + xlab("Date") +
      geom_line(alpha=0.8,aes(colour = Pays,group = 1,
                                     text = paste0(format(Date, "%d %B %Y"), "\n", Pays, ": ",newCases))) +
      ylab("Nouveaux cas") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "right", plot.title = element_text(size=10))
    ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  #graphique comparaison décès pays
  comparaison_pays_plotD <- function(df_deaths,start_date){
    g <-  df_deaths %>%
      ggplot(aes(x =Date, y=trend)) + 
      xlim(c(start_date,(current_date+5))) + xlab("Date") +
      geom_line(alpha=0.8,aes(colour = Pays,group = 1,
                              text = paste0(format(Date, "%d %B %Y"), "\n", Pays, ": ",newDeaths))) +
      ylab("Nouveaux décès") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "right", plot.title = element_text(size=10))
    ggplotly(g, tooltip = c("text")) 
  }
  
  #graphique comparaison infections regions
  comparaison_region_plotC <- function(df_cases,start_date){
    g <-  df_cases %>%
      ggplot(aes(x =jour, y=trend)) + 
      xlim(c(start_date,(current_date+5))) + xlab("Date") +
      geom_line(alpha=0.8,aes(colour = nom_region,group = 1,
                              text = paste0(format(jour, "%d %B %Y"), "\n", nom_region, ": ",P))) +
      ylab("Nouveaux cas") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "right", plot.title = element_text(size=10))
    ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  #graphique comparaison tests regions
  comparaison_region_plotT <- function(df_cases,start_date){
    g <-  df_cases %>%
      ggplot(aes(x =jour, y=trend)) + 
      xlim(c(start_date,(current_date+5))) + xlab("Date") +
      geom_line(alpha=0.8,aes(colour = nom_region,group = 1,
                              text = paste0(format(jour, "%d %B %Y"), "\n", nom_region, ": ",T))) +
      ylab("Nouveaux cas") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "right", plot.title = element_text(size=10))
    ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  #graphique comparaison infections departements
  comparaison_departement_plotC <- function(df_cases,start_date){
    g <-  df_cases %>%
      ggplot(aes(x =jour, y=trend)) + 
      xlim(c(start_date,(current_date+5))) + xlab("Date") +
      geom_line(alpha=0.8,aes(colour = nom_departement,group = 1,
                              text = paste0(format(jour, "%d %B %Y"), "\n", nom_departement, ": ",P))) +
      ylab("Nouveaux cas") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "right", plot.title = element_text(size=10))
    ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  #graphique comparaison tests departements
  comparaison_departement_plotT <- function(df_cases,start_date){
    g <-  df_cases %>%
      ggplot(aes(x =jour, y=trend)) + 
      xlim(c(start_date,(current_date+5))) + xlab("Date") +
      geom_line(alpha=0.8,aes(colour = nom_departement,group = 1,
                              text = paste0(format(jour, "%d %B %Y"), "\n", nom_departement, ": ",T))) +
      ylab("Nouveaux cas") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "right", plot.title = element_text(size=10))
    ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  #graphique comparaison hospitalisations regions
  comparaison_region_plotH <- function(df_hosps,start_date){
    g <-  df_hosps %>%
      ggplot(aes(x =jour, y=trend)) + 
      xlim(c(start_date,(current_date+5))) + xlab("Date") +
      geom_line(alpha=0.8,aes(colour = nom_region,group = 1,
                              text = paste0(format(jour, "%d %B %Y"), "\n", nom_region, ": ",hosp))) +
      ylab("Hospitalisations") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "right", plot.title = element_text(size=10))
    ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  #graphique comparaison hospitalisations departements
  comparaison_departement_plotH <- function(df_hosps,start_date){
    g <-  df_hosps %>%
      ggplot(aes(x =jour, y=trend)) + 
      xlim(c(start_date,(current_date+5))) + xlab("Date") +
      geom_line(alpha=0.8,aes(colour = nom_departement,group = 1,
                              text = paste0(format(jour, "%d %B %Y"), "\n", nom_departement, ": ",hosp))) +
      ylab("Hospitalisations") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "right", plot.title = element_text(size=10))
    ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  #graphique comparaison reanimations regions
  comparaison_region_plotR <- function(df_rea,start_date){
    g <-  df_rea %>%
      ggplot(aes(x =jour, y=trend)) + 
      xlim(c(start_date,(current_date+5))) + xlab("Date") +
      geom_line(alpha=0.8,aes(colour = nom_region,group = 1,
                              text = paste0(format(jour, "%d %B %Y"), "\n", nom_region, ": ",rea))) +
      ylab("Réanimations") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "right", plot.title = element_text(size=10))
    ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  #graphique comparaison reanimations departements
  comparaison_departement_plotR <- function(df_rea,start_date){
    g <-  df_rea %>%
      ggplot(aes(x =jour, y=trend)) + 
      xlim(c(start_date,(current_date+5))) + xlab("Date") +
      geom_line(alpha=0.8,aes(colour = nom_departement,group = 1,
                              text = paste0(format(jour, "%d %B %Y"), "\n", nom_departement, ": ",rea))) +
      ylab("Réanimations") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "right", plot.title = element_text(size=10))
    ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  #graphique comparaison décès regions
  comparaison_region_plotD <- function(df_dc,start_date){
    g <-  df_dc %>%
      ggplot(aes(x =jour, y=trend)) + 
      xlim(c(start_date,(current_date+5))) + xlab("Date") +
      geom_line(alpha=0.8,aes(colour = nom_region,group = 1,
                              text = paste0(format(jour, "%d %B %Y"), "\n", nom_region, ": ",nv_dc))) +
      ylab("Décès") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "right", plot.title = element_text(size=10))
    ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  #graphique comparaison décès departements
  comparaison_departement_plotD <- function(df_dc,start_date){
    g <-  df_dc %>%
      ggplot(aes(x =jour, y=trend)) + 
      xlim(c(start_date,(current_date+5))) + xlab("Date") +
      geom_line(alpha=0.8,aes(colour = nom_departement,group = 1,
                              text = paste0(format(jour, "%d %B %Y"), "\n", nom_departement, ": ",nv_dc))) +
      ylab("Décès") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "right", plot.title = element_text(size=10))
    ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  #--------------------------------------------------------------------------------------------------
  ###graphiques monde ---
  # fonctions de traçage des courbes et histogrammes liés aux cartes 
  # elles prennent en parametres l'id de pays,region ou departement sur la carte
  
  #courbe des cas dans le monde
  graphique <- function(id){
    
    dataPays <- reactive({ filter(casesP(),Country == id) })
    p <- components(dataPays())  %>%
      as_tsibble() %>%
      autoplot(trend, colour ="darkorange3",size = 1) +
      scale_y_continuous(n.breaks = 5,labels = scales::number)+
      scale_x_date(breaks = "3 month",minor_breaks = "1 month",date_labels = "%b %Y") + 
      theme(axis.text.x = element_text(vjust = 0.5,hjust = 0.5),legend.title=element_text(size=2)) +
      labs(title = dataPays()$Pays,
           subtitle = "Nouveaux Cas",
           y = "Depuis le début",
           x = "Date") + theme(axis.title.x = element_blank())
    
    return(p)
  }
  
  # histograme des cas dans le monde
  histogram <- function(id){
    dataPays30 <- reactive({ filter(casesPRecent(),Country == id) })
    
    h <- dataPays30() %>%
      ggplot(aes(x=Date,weight = newCases)) +
      geom_histogram(color="black",fill = "darkorange3") +
      scale_y_continuous(n.breaks = 5,labels = scales::number)+
      scale_x_date(breaks = "5 day",minor_breaks = "1 day",date_labels = "%d %b") + 
      theme(axis.text.x = element_text(vjust = 0.5,hjust = 0.5),legend.title=element_text(size=2)) +
      labs(
        y = paste("30 derniers jours"),
        x = "Date")
    return(h)
  }
  
  # fonction qui prend en parametre l'id de pays et combine dans un même graphique la courbe et l'histogramme
  popupContent <- function(id){
    gr <- graphique(id)
    h <- histogram(id)
    ph <- grid.arrange(gr,h)
    return(ph)
  }
  
  # courbe des décès dans le monde
  graphiqueD <- function(id){
    
    dataPays <- reactive({ filter(deathsP(), Code == id) })
    p <- components(dataPays())  %>%
      as_tsibble() %>%
      autoplot(trend, colour =lkp_grey,size = 1) +
      scale_y_continuous(n.breaks = 5,labels = scales::number)+
      scale_x_date(breaks = "3 month",minor_breaks = "1 month",date_labels = "%b %Y") + 
      theme(axis.text.x = element_text(vjust = 0.5,hjust = 0.5)) +
      labs(title = dataPays()$Pays,
           subtitle = "Nouveaux décès",
           y = "Depuis le début",
           x = "Date") + theme(axis.title.x = element_blank(),legend.title=element_text(size=2))
    
    return(p)
  }
  
  #histogramme des décès dans le monde
  histogramD <- function(id){
    dataPays30 <- reactive({ filter(deathsPRecent(),Code == id) })
    
    h <- dataPays30() %>%
      ggplot(aes(x=Date,weight = newDeaths)) +
      geom_histogram(color="black",fill = lkp_grey) +
      scale_y_continuous(n.breaks = 5,labels = scales::number)+
      scale_x_date(breaks = "5 day",minor_breaks = "1 day",date_labels = "%d %b") + 
      theme(axis.text.x = element_text(vjust = 0.5,hjust = 0.5),legend.title=element_text(size=2)) +
      labs(
        y = "30 derniers jours",
        x = "Date") 
    return(h)
  }
  
  ### graphique total dep, region --- 
  
  # fonction de traçage des courbes simples pour les départements et régions 
  #( courbe sans facteur de sexe ou classe d'âge) 
  # elle prend en parametre soit dep ou reg, l'indicateur (infections,deces etc..),
  # la couleur utilisée, et l'id de departement ou region
  # elle retourne la courbe voulue
  graphiqueDepReg <- function(data,depOUreg,indicateur,color,id){
    if(depOUreg == "dep"){
      dataF <- reactive({ filter(data,nom_departement == id)})
    }else {
      dataF <- reactive({ filter(data,nom_region == id)})
    }
    
    p <-  components(dataF()) %>%
      as_tsibble() %>%
      autoplot(trend,colour = color,size = 1) + 
      scale_y_continuous(n.breaks = 5,labels = scales::number)+
      scale_x_date(breaks = "3 month",minor_breaks = "1 month",date_labels = "%b %Y") + 
      theme(axis.text.x = element_text(vjust = 0.5,hjust = 0.5),axis.title.x = element_blank()) +
      labs(title = id,
           y = "Depuis le début",
           x = "Date")
    return(p)
  }
  
  ### graphique factor dep, region --- 
  
  # fonction de traçage des courbes avec facteur pour les départements et régions 
  #(sexe ou classe d'âge) 
  # elle prend en parametre soit dep ou reg, l'indicateur (infections,deces etc..),
  # le facteur, et l'id de departement ou region
  graphiqueDepRegFactor <- function(data,depOUreg,indicateur,factor,id){
    
    if(depOUreg == "dep"){
      dataF <- reactive({ filter(data,nom_departement == id)})
    }else {
      dataF <- reactive({ filter(data,nom_region == id)})
    }
    p <- components(dataF()) %>%
      as_tsibble() %>%
      ggplot(aes(x =jour, y=trend))
    if(factor == "cl_age"){
      p <- p +   
        geom_line(aes(colour = cl_age90),size = 1) +
        scale_color_brewer(palette="Spectral") 
    }else {
      p <- p +   
        geom_line(aes(colour = factor(sexe)),size = 1) +
        scale_color_manual(values = c(lkp_blueHomme,"firebrick3"))
    }
    
    p <- p + 
      scale_y_continuous(n.breaks = 5,labels = scales::number)+
      scale_x_date(breaks = "3 month",minor_breaks = "1 month",date_labels = "%b %Y") + 
      theme(axis.text.x = element_text(vjust = 0.5,hjust = 0.5),axis.title.x = element_blank(),
            legend.position = "none") +
      labs(title = id,
           y = indicateur)
    return(p)
  }
  
  ### histogram total dep, region --- 
  
  # fonction de traçage des histogrammes simples pour les départements et régions 
  #(sans facteur de sexe ou classe d'âge) 
  # elle prend en parametre soit dep ou reg, l'indicateur (infections,deces etc..),
  # le facteur, et l'id de departement ou region
  histogramDepReg <- function(data,depOUreg,indicateur,color,id,factor){
    if(depOUreg == "dep"){
      dataF <- reactive({ filter(data,nom_departement == id)})
    }else {
      dataF <- reactive({ filter(data,nom_region == id)})
    }
    
    if(factor == "sexe"){
      if(indicateur == "Infections"){
        p <-  dataF() %>%
          ggplot(aes(x=jour,weight = P,fill = sexe)) + geom_histogram(color="black")
      }else if (indicateur == "Tests"){
        p <-  dataF() %>%
          ggplot(aes(x=jour,weight = T,fill = sexe)) + geom_histogram(color="black")
      }else if(indicateur == "Hospitalisations"){
        p <- dataF() %>% 
          ggplot(aes(x=jour,weight = hosp,fill = sexe)) + geom_histogram(color="black")
      }else if (indicateur == "Réanimations"){
        p <- dataF() %>%
          ggplot(aes(x=jour,weight = rea,fill = sexe)) + geom_histogram(color="black")
      }else{
        p <- dataF() %>%
          ggplot(aes(x=jour,weight = nv_dc,fill = sexe)) + geom_histogram(color="black")
      }
      p <- p + scale_fill_manual("Sexe",values = c(lkp_blueHomme,"firebrick3"))
      
    }else if(factor == "cl_age"){
      if(indicateur == "Infections"){
        p <-  dataF() %>%
          ggplot(aes(x=jour,weight = P,fill = cl_age90)) + geom_histogram(color="black")
      }else if (indicateur == "Tests"){
        p <-  dataF() %>%
          ggplot(aes(x=jour,weight = T,fill = cl_age90))+ geom_histogram(color="black")
      }else if(indicateur == "Hospitalisations"){
        p <- dataF() %>%
          ggplot(aes(x=jour,weight = hosp,fill = cl_age90)) + geom_histogram(color="black")
      }else if (indicateur == "Réanimations"){
        p <- dataF() %>%
          ggplot(aes(x=jour,weight = rea,fill = cl_age90)) + geom_histogram(color="black")
      }else{
        p <- dataF() %>%
          ggplot(aes(x=jour,weight = nv_dc,fill = cl_age90))+ geom_histogram(color="black")
      }
      p <- p + scale_fill_brewer("Classe d'âge",palette="Spectral") 
    }else{
      if(indicateur == "Infections"){
        p <-  dataF() %>%
          ggplot(aes(x=jour,weight = P)) + geom_histogram(color="black",fill = color)
      }else if (indicateur == "Tests"){
        p <-  dataF() %>%
          ggplot(aes(x=jour,weight = T)) + geom_histogram(color="black",fill = color)
      }else if(indicateur == "Hospitalisations"){
        p <- dataF() %>%
          ggplot(aes(x=jour,weight = hosp)) + geom_histogram(color="black",fill = color)
      }else if (indicateur == "Réanimations"){
        p <- dataF() %>%
          ggplot(aes(x=jour,weight = rea)) + geom_histogram(color="black",fill = color)
      }else{
        p <- dataF() %>%
          ggplot(aes(x=jour,weight = nv_dc)) + geom_histogram(color="black",fill = color)
      }
    }
  
    p <- p +
      scale_y_continuous(n.breaks = 5,labels = scales::number)+
      scale_x_date(breaks = "5 day",minor_breaks = "1 day",date_labels = "%d %b") + 
      theme(axis.text.x = element_text(vjust = 0.5,hjust = 0.5)) +
      labs(
           y = "30 derniers jours",
           x = "Date") + theme(legend.position = "top")

    return(p)
  }
  #------------------------------------------------------------------------------------------
  ##data departements cas tests --
  
  # import des données reactives des cas departements
  dataDep <- reactiveFileReader(7200000,session,"https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675",readFunc =  read_delim,delim = ";") 
  
  # données reactives cas departements par age
  dataDepartementCasAges <- reactive({
    dataDep() %>%
      filter(dep<=95) %>%
      mutate(dep = as.factor(dep),cl_age90 = fct_rev(factor(cl_age90,levels = c(09,19,29,39,49,59,69,79,89,90),
                                                            labels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79",
                                                                       "80-89","supérier à 90")))) %>%
      left_join(departements,by = "dep") %>%
      left_join(centresDep,by = "nom_departement") %>%
      filter(cl_age90 != 0)
  })
  
  # données reactives cas departements total (sans facteur)
  dataDepartementCasTotal <- reactive({
    dataDep() %>%
      buildCasesDep(departements,centresDep)
  })
  
  # indicateurs des infections depuis de debut de l'épidemie ( total)
  dataDepartementCasTotalFinal <- reactive({
    dataDepartementCasTotal() %>% 
      group_by(nom_departement,dep,latitude,longitude) %>%
      summarise(P = sum(P), T = sum(T), pop = mean(pop),tauxIncidence = sum(P)/mean(pop)*100000,
                tauxPositivite = sum(P)/sum(T)*100000,tauxTests = sum(T)/mean(pop)*100000)
  })
  
  # dataset pour tracer l'histogram cas et test departements total et par age
  dataDepartementCasRecent <- reactive({
    dataDepartementCasAges() %>%
      filter(jour>max(dataDepartementCasTotal()$jour)+days(-30))
  })
  
  #indicateurs infections de 30 derniers jours ( popup )
  
  dataDepartementCasTotalRecent <- reactive({
    dataDepartementCasRecent() %>%
      group_by(dep,nom_departement,latitude,longitude) %>%
      summarise(P = sum(P),T=sum(T),pop = mean(pop),tauxIncidence = sum(P)/mean(pop)*100000,
                tauxPositivite = sum(P)/sum(T)*100000,tauxTests = sum(T)/mean(pop)*100000)
  })
  
  # dataset pour tracer le graphique cas departement total
  dataCasDepP <- reactive({
    dataDepartementCasTotal() %>% 
      as_tsibble(key = c(nom_departement,dep),index = jour) %>% 
      model(classical_decomposition(P,type = "multiplicative"))
  })
  

  
  # dataset pour tracer le grapique cas departement par age
  dataCasDepPAges <- reactive({
    dataDepartementCasAges() %>% 
      as_tsibble(key = c(nom_departement,dep,cl_age90),index = jour) %>% 
      model(classical_decomposition(P,type = "multiplicative"))
  })
  
  # dataset pour tracer le graphique test departement total
  
  dataTestDepP <- reactive({
    dataDepartementCasTotal() %>% 
      as_tsibble(key = c(nom_departement,dep),index = jour) %>% 
      model(classical_decomposition(T,type = "multiplicative"))
  })
  
  # dataset pour tracer le graphique tests departements par age
  
  dataTestDepPAges <- reactive({
    dataDepartementCasAges() %>% 
      as_tsibble(key = c(nom_departement,dep,cl_age90),index = jour) %>% 
      model(classical_decomposition(T,type = "multiplicative"))
  })
  
  ##data departements hosp rea dc --
  # données reactives des hospitalisations par département
  dataDepHosp <- reactiveFileReader(7200000,session,"https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7",
                                    readFunc =  read_delim,delim = ";")
  # données des hosps par départements totales (sans facteur)
  dataDepHospTotal <- reactive({
    dataDepHosp() %>%
    buildHospDep(departements,centresDep)
  })
  
  # données des hosps par département par sexe
  dataDepartementHospSexe <- reactive({
    dataDepHosp() %>%
      left_join(depPop,by="dep") %>%
      filter(dep<=95) %>%
      mutate(dep = as.factor(dep),sexe = factor(sexe,levels = c(1,2),labels = c("Hommes","Femmes")),dc = as.integer(dc)) %>%
      left_join(departements,by = "dep") %>%
      left_join(centresDep,by = "nom_departement") %>%
      filter(sexe != 0) %>%
      group_by(nom_departement,dep,sexe,longitude,latitude) %>%
      mutate(nv_dc = diffDI(dc) %>% pmax(0),nv_hosp = diffDI(hosp) %>% pmax(0),nv_rea = diffDI(rea) %>% pmax(0))
  })
  
  # indicateurs depuis de debut de l'épidemie ( total) des hosps par département
  dataDepartementHospTotalFinal <- reactive({
    dataDepHospTotal() %>% 
      group_by(dep,nom_departement,latitude,longitude) %>%
      summarise(hosp = sum(hosp),rea = sum(rea),rad = sum(rad),nv_dc = sum(nv_dc),pop=mean(pop)
                ,tauxHosps = sum(nv_hosp)/mean(pop)*100000,tauxDeces = sum(nv_dc)/mean(pop)*100000,tauxRea = sum(nv_rea)/mean(pop)*100000,tauxDeces = sum(nv_dc)/mean(pop)*100000,nv_hosp = sum(nv_hosp),nv_rea = sum(nv_rea))
  })
  
  #dataset pour tracer l'histogram hospitalisations reanimations décès par departement total et par sexe
  
  dataDepartementHospRecent <- reactive({
    dataDepartementHospSexe() %>%
      filter(jour>max(dataDepHospTotal()$jour)+days(-30))
  })
  
  #indicateurs de 30 derniers jours ( popup ) pour hosps par département à utiliser pour chiffres clés 
  
  dataDepartementHospTotalRecent <- reactive({
    dataDepartementHospRecent() %>%
      group_by(dep,nom_departement,latitude,longitude) %>%
      summarise(hosp = sum(hosp),rea = sum(rea),rad = sum(rad),nv_dc = sum(nv_dc),pop=mean(pop)
                ,tauxHosps = sum(nv_hosp)/mean(pop)*100000,tauxDeces = sum(nv_dc)/mean(pop)*100000,tauxDeces = sum(nv_dc)/mean(pop)*100000,tauxRea = sum(nv_rea)/mean(pop)*100000,nv_hosp = sum(nv_hosp),nv_rea = sum(nv_rea))
  })
  
  #dataset pour tracer le graphique hospitalisations par departement total
  dataHospDepP <- reactive({
    dataDepHospTotal() %>%
      as_tsibble(key = c(nom_departement,dep),index = jour) %>% 
      model(classical_decomposition(hosp,type = "multiplicative"))
  })
  
  #dataset pour tracer le graphique reanimations par departement total
  dataReaDepP <- reactive({
    dataDepHospTotal() %>%
      as_tsibble(key = c(nom_departement,dep),index = jour) %>% 
      model(classical_decomposition(rea,type = "multiplicative"))
  })
  
  #dataset pour tracer le graphique décès par departement total
  dataDecesDepP <- reactive({
    dataDepHospTotal() %>%
      as_tsibble(key = c(nom_departement,dep),index = jour) %>% 
      model(classical_decomposition(nv_dc,type = "multiplicative"))
  })
  
  #dataset pour tracer le graphique hospitalisations par departement par sexe
  dataHospDepPSexe <- reactive({
    dataDepartementHospSexe() %>%
      as_tsibble(key = c(nom_departement,dep,sexe),index = jour) %>% 
      model(classical_decomposition(hosp,type = "multiplicative"))
  })
  
  #dataset pour tracer le graphique reanimations par departement par sexe
  dataReaDepPSexe <- reactive({
    dataDepartementHospSexe() %>%
      as_tsibble(key = c(nom_departement,dep,sexe),index = jour) %>% 
      model(classical_decomposition(rea,type = "multiplicative"))
  })
  
  #dataset pour tracer le graphique décès par departement par sexe
  dataDecesDepPSexe <- reactive({
    dataDepartementHospSexe() %>%
      as_tsibble(key = c(nom_departement,dep,sexe),index = jour) %>% 
      model(classical_decomposition(nv_dc,type = "multiplicative"))
  })
  
  ## data regions cas tests --- 
  # données reactives des infections et tests par region
  dataReg <- reactiveFileReader(7200000,session,"https://www.data.gouv.fr/fr/datasets/r/001aca18-df6a-45c8-89e6-f82d689e6c01",
                                readFunc =  read_delim,delim = ";")

  # données des cas et tests par region par age
  dataRegionCasAges <- reactive({
    dataReg() %>%
      mutate(reg = as.factor(reg),cl_age90 = fct_rev(factor(cl_age90,levels = c(09,19,29,39,49,59,69,79,89,90),
                                                            labels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79",
                                                                       "80-89","supérier à 90")))) %>%
      left_join(regions,by = "reg") %>%
      left_join(centresReg,by = "nom_region") %>%
      filter(cl_age90 != 0)
  })
  
  #données des cas et tests par region par sexe
  dataRegionCasSexe <- reactive({
    dataReg() %>%
    mutate(reg = as.factor(reg)) %>%
      left_join(regions,by = "reg") %>%
      left_join(centresReg,by = "nom_region") %>%
      group_by(reg,nom_region,jour) %>%
      summarise(latitude = mean(latitude),longitude = sum(longitude),P = sum(P),T=sum(T),
                P_h = sum(P_h),P_f = sum(P_f),T_h = sum(T_h),T_f = sum(T_f),pop = mean(pop))
  })
  
  # données pour tracer le graphique des cas par sexe par region
  dataRegionCasSexeP <- reactive({
    dataRegionCasSexe() %>%
      select(!c(P,T,T_h,T_f)) %>%
      pivot_longer(c(P_h,P_f),names_to = "sexe",values_to = "P") %>%
      mutate(sexe = str_replace(sexe,"P_h","1"),sexe = str_replace(sexe,"P_f","2") ) %>%
      mutate(sexe = factor(sexe,levels = c(1,2),labels = c("Hommes","Femmes")))
  })

  # données pour tracer le graphique des tests par sexe par region
  dataRegionCasSexeT <- reactive({
    dataRegionCasSexe() %>%
      select(!c(P,T,P_h,P_f)) %>%
      pivot_longer(c(T_h,T_f),names_to = "sexe",values_to = "T") %>%
      mutate(sexe = str_replace(sexe,"T_h","1"),sexe = str_replace(sexe,"T_f","2") ) %>%
      mutate(sexe = factor(sexe,levels = c(1,2),labels = c("Hommes","Femmes")))
  })
  
  # données pour tracer le graphique des cas totals par region
  dataRegionCasTotal <- reactive({
    dataReg() %>%
      buildCasesReg(regions,centresReg)
  })
  
  # indicateurs depuis de debut de l'épidemie ( total) des cas par region chiffres clés 
  
  dataRegionCasTotalFinal <- reactive({
    date_diff_cas <- as.integer(gsub("[a-zA-Z,0H,0M,0S]", "", as.character(days(max(dataRegionCasTotal()$jour) - min(dataRegionCasTotal()$jour)))))
    nb_months_cas <- date_diff_cas/30
    dataRegionCasTotal() %>%
      group_by(nom_region,reg,latitude,longitude) %>%
      summarise(P = sum(P), T = sum(T), pop = mean(pop),tauxIncidence = (sum(P)/mean(pop)*100000)/nb_months_cas,
                tauxPositivite = (sum(P)/sum(T)*100000)/nb_months_cas,tauxTests = (sum(T)/mean(pop)*100000)/nb_months_cas)
  })
  
  #dataset pour tracer l'histogram cas et test total par region et par age (30 derniers jours)
  dataRegionCasRecent <- reactive({
    dataRegionCasAges() %>%
      filter(jour>max(dataRegionCasTotal()$jour)+days(-30))
  })
  
  #dataset pour tracer l'histogram cas par sexe (30 derniers jours)
  dataRegionCasRecentSexe <- reactive({
    dataRegionCasSexeP() %>%
      filter(jour>max(dataRegionCasTotal()$jour)+days(-30))
  })
  
  # pour tracer l'histogram tests par sexe (30 derniers jours)
  dataRegionTestRecentSexe <- reactive({
    dataRegionCasSexeT() %>%
      filter(jour>max(dataRegionCasTotal()$jour)+days(-30))
  })
  
  
  #indicateurs de 30 derniers jours ( popup ) des cas par region chiffres clés
  dataRegionCasTotalRecent <- reactive({
    dataRegionCasRecent() %>%
      group_by(reg,nom_region,latitude,longitude) %>%
      summarise(P = sum(P),T=sum(T),pop = mean(pop),tauxIncidence = sum(P)/mean(pop)*100000,
                tauxPositivite = sum(P)/sum(T)*100000,tauxTests = sum(T)/mean(pop)*100000)
  })
  
  # pour tracer le graphique des cas totals par region 
  dataCasRegP <- reactive({
    dataRegionCasTotal() %>% 
      as_tsibble(key = c(nom_region,reg),index = jour) %>% 
      model(classical_decomposition(P,type = "multiplicative"))
  })
  
  #dataset pour tracer le graphiques de comparaison cas par regions
  CasesR_comparaison <- reactive({
    components(dataCasRegP() %>% filter(nom_region %in% input$region_select)) %>% as_tsibble()
  })
  
  # pour comparaison cas par departements
  CasesD_comparaison <- reactive({
    components(dataCasDepP() %>% filter(nom_departement %in% input$departement_select)) %>% as_tsibble()
  })
  
  # pour comparaison tests par regions
  TestsR_comparaison <- reactive({
    components(dataTestRegP() %>% filter(nom_region %in% input$region_select)) %>% as_tsibble()
  })
  
  # pour comparaison test par departements
  TestsD_comparaison <- reactive({
    components(dataTestDepP() %>% filter(nom_departement %in% input$departement_select)) %>% as_tsibble()
  })
  
  # pour comparaison hospitalisations par region
  HospsR_comparaison <- reactive({
    components(dataHospRegP() %>% filter(nom_region %in% input$region_select)) %>% as_tsibble()
  })
  
  # pour comparaison hosps par departement
  HospsD_comparaison <- reactive({
    components(dataHospDepP() %>% filter(nom_departement %in% input$departement_select)) %>% as_tsibble()
  })
  
  # pour comparaison reanimations par region
  ReaR_comparaison <- reactive({
    components(dataReaRegP() %>% filter(nom_region %in% input$region_select)) %>% as_tsibble()
  })
  
  # pour comparaison reanimations par departements
  ReaD_comparaison <- reactive({
    components(dataReaDepP() %>% filter(nom_departement %in% input$departement_select)) %>% as_tsibble()
  })
  
  # pour comparaison décès par regions
  DeathsR_comparaison <- reactive({
    components(dataDecesRegP() %>% filter(nom_region %in% input$region_select)) %>% as_tsibble()
  })
  
  # pour comparaison décès par departements
  DeathsD_comparaison <- reactive({
    components(dataDecesDepP() %>% filter(nom_departement %in% input$departement_select)) %>% as_tsibble()
  })
  
  # pour tracer le graphique cas par region par age
  dataCasRegPAges <- reactive({
    dataRegionCasAges() %>% 
      as_tsibble(key = c(nom_region,reg,cl_age90),index = jour) %>% 
      model(classical_decomposition(P,type = "multiplicative"))
  })
  
  # pour tracer le graphique tests totals par region
  
  dataTestRegP <- reactive({
    dataRegionCasTotal() %>% 
      as_tsibble(key = c(nom_region,reg),index = jour) %>% 
      model(classical_decomposition(T,type = "multiplicative"))
  })
  
  # pour tracer le graphique tests par region par age
  
  dataTestRegPAges <- reactive({
    dataRegionCasAges() %>% 
      as_tsibble(key = c(nom_region,reg,cl_age90),index = jour) %>% 
      model(classical_decomposition(T,type = "multiplicative"))
  })
  
  
  # pour tracer le graphique cas par region par sexe
  dataCasRegPSexe <- reactive({
    dataRegionCasSexeP() %>% 
      as_tsibble(key = c(nom_region,reg,sexe),index = jour) %>% 
      model(classical_decomposition(P,type = "multiplicative"))
  })
  
  # pour tracer le graphique tests par region par sexe
  dataTestRegPSexe <- reactive({
    dataRegionCasSexeT() %>% 
      as_tsibble(key = c(nom_region,reg,sexe),index = jour) %>% 
      model(classical_decomposition(T,type = "multiplicative"))
  })
  

  ##data regions hosp rea dc --
  # données reactives des hospitalisations et reanimations et décès par region
  dataRegHosp <- reactiveFileReader(7200000,session,"https://www.data.gouv.fr/fr/datasets/r/08c18e08-6780-452d-9b8c-ae244ad529b3",
                                    readFunc =  read_delim,delim = ";")
  dataRegHospTotal <- reactive({
    dataRegHosp() %>%
      buildHospReg(regions,centresReg)
  })
  
  # données des hospitalisations par region par age
  dataRegionHospAges <- reactive({
    dataRegHosp() %>%
      mutate(reg = as.factor(reg),cl_age90 = fct_rev(factor(cl_age90,levels = c(09,19,29,39,49,59,69,79,89,90),
                                                                       labels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79",
                                                                                  "80-89","supérier à 90")))) %>%
      left_join(regions,by = "reg") %>%
      left_join(centresReg,by = "nom_region") %>%
      left_join(regPop,by="reg") %>%
      filter(cl_age90 != 0) %>%
      group_by(nom_region,reg,cl_age90,longitude,latitude) %>%
      mutate(nv_dc = diffDI(dc),nv_hosp = diffDI(hosp) %>% pmax(0),nv_rea = diffDI(rea) %>% pmax(0))
  })
  
  # indicateurs depuis de debut de l'épidemie ( total) pour hosps et reanimations et deces par region chiffres clés
  dataRegionHospTotalFinal <- reactive({
    date_diff_hosp <- as.integer(gsub("[a-zA-Z,0H,0M,0S]", "", as.character(days(max(dataRegHospTotal()$jour) - min(dataRegHospTotal()$jour)))))
    nb_months_hosp <- date_diff_hosp/30
    
    dataRegHospTotal() %>% 
      group_by(reg,nom_region,latitude,longitude) %>%
      summarise(hosp = sum(hosp),rea = sum(rea),rad = sum(rad),nv_dc = sum(nv_dc),pop=mean(pop)
                ,tauxHosps = (sum(nv_hosp)/mean(pop)*100000)/nb_months_hosp,tauxDeces = (sum(nv_dc)/mean(pop)*100000)/nb_months_hosp,tauxRea = (sum(nv_rea)/mean(pop)*100000)/nb_months_hosp,nv_hosp = sum(nv_hosp),nv_rea = sum(nv_rea))
  })
  
  # pour tracer l'histogram hosp reanimations décès totals par region et par age
  
  dataRegionHospRecent <- reactive({
    dataRegionHospAges() %>%
      filter(jour>max(dataRegHospTotal()$jour)+days(-30))
  })
  
  #indicateurs de 30 derniers jours pour hospitalisations totales ( popup ) chiffres clés
  
  dataRegionHospTotalRecent <- reactive({
    dataRegionHospRecent() %>%
      group_by(reg,nom_region,latitude,longitude) %>%
      summarise(hosp = sum(hosp),rea = sum(rea),rad = sum(rad),nv_dc = sum(nv_dc),pop=mean(pop)
                ,tauxHosps = sum(nv_hosp)/mean(pop)*100000,tauxDeces = sum(nv_dc)/mean(pop)*100000,tauxDeces = sum(nv_dc)/mean(pop)*100000,tauxRea = sum(nv_rea)/mean(pop)*100000,nv_hosp = sum(nv_hosp),nv_rea = sum(nv_rea))
  })
  
  # dataset pour tracer le graphique hosps totals par region
  dataHospRegP <- reactive({
    dataRegHospTotal() %>%
      as_tsibble(key = c(nom_region,reg),index = jour) %>% 
      model(classical_decomposition(hosp,type = "multiplicative"))
  })
  
  # dataset pour tracer le graphique reanimations totals par region
  dataReaRegP <- reactive({
    dataRegHospTotal() %>%
      as_tsibble(key = c(nom_region,reg),index = jour) %>% 
      model(classical_decomposition(rea,type = "multiplicative"))
  })
  
  # dataset pour tracer le graphique des deces totals par region
  dataDecesRegP <- reactive({
    dataRegHospTotal() %>%
      as_tsibble(key = c(nom_region,reg),index = jour) %>% 
      model(classical_decomposition(nv_dc,type = "multiplicative"))
  })
  
  # dataset pour tracer le graphique hosp par region par age
  dataHospRegPAges <- reactive({
    dataRegionHospAges() %>%
      as_tsibble(key = c(nom_region,reg,cl_age90),index = jour) %>% 
      model(classical_decomposition(hosp,type = "multiplicative"))
  })
  
  
  # dataset pour tracer le graphique reanimations par region par age
  dataReaRegPAges <- reactive({
    dataRegionHospAges() %>%
      as_tsibble(key = c(nom_region,reg,cl_age90),index = jour) %>% 
      model(classical_decomposition(rea,type = "multiplicative"))
  })
  
  # dataset pour tracer le graphique des deces par region par age
  dataDecesRegPAges <- reactive({
    dataRegionHospAges() %>%
      as_tsibble(key = c(nom_region,reg,cl_age90),index = jour) %>% 
      model(classical_decomposition(nv_dc,type = "multiplicative"))
  })
  
  
  # maps 
  # carte de monde
  output$map <- renderLeaflet({
    
    palInfection <- colorBin( "Oranges", bins=c(0,30,60,80,100,200,400,800,1000,1500,2000,4500), na.color = "#aaff56")
    palDeces <- colorBin("Greys", bins=c(0,1,2,3,4,5,10,15,20,30,50,60,80), na.color = "#aaff56")
    
    leaflet(DeathsL()) %>% 
      addTiles() %>% 
      addLayersControl(
        position = "topleft",
        overlayGroups = c("Infections","Décès"),
        options = layersControlOptions(collapsed = FALSE))  %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addLegend("bottomright", pal = palInfection, values = ~CasesL()$tauxInfection,labFormat = labelFormat(big.mark = " "),
                title = "<center><em>Taux d'incidence<br/>pour les 30 derniers jours <br/> par 100 000 habitants</em></center>",group = "Infections") %>%
      addLegend("bottomright", pal = palDeces, values = ~DeathsL()$tauxDeces,
                title = "<center><em>Taux de décès<br/>pour les 30 derniers jours <br/> par 100 000 habitants </em></center>",group = "Décès") %>%
      hideGroup(c("Décès")) %>%
      setView(lng = 5.081666, lat = 17.607789,zoom = 2.4)
  })
  
  # contenus des textes des chiffres clés
  # chiffres clés des 30 derniers jours
  output$jours <- renderText({
    paste0("Au cours les 30 derniers jours")
  })
  output$monde_cases_count <- renderText({
    paste0(numberF(sum(casesPRecent()$newCases)), "    infections")
  })
  output$monde_deaths_count <- renderText({
    paste0(numberF(sum(deathsPRecent()$newDeaths)), "   décès")
  })
  
  # chiffres clés depuis le début de l'épidémie
  output$debut <- renderText({
    paste0("Depuis le début de l'épidémie")
  })
  output$mondeD_cases_count <- renderText({
    paste0(numberF(sum(Cases()$newCases)), "    infections")
  })
  output$mondeD_deaths_count <- renderText({
    paste0(numberF(sum(Deaths()$newDeaths)), "   décès")
  })
  
  output$test <- renderText({
    input$country_select
  })
  
  # le graphique de comparaison des pays 
  output$comparaison_pays <- renderPlotly({
    if(input$outcome_select == "Infections")
      comparaison_pays_plot(Cases_comparaison(),input$minimum_date)
    else 
      comparaison_pays_plotD(Deaths_comparaison(),input$minimum_date)
  })
  
  # le graphique de comparaison des regions
  output$comparaison_region <- renderPlotly({
    if(input$outcome_selectR == "Infections")
      comparaison_region_plotC(CasesR_comparaison(),input$minimum_dateR)
    else if(input$outcome_selectR == "Tests")
      comparaison_region_plotT(TestsR_comparaison(),input$minimum_dateR)
    else if(input$outcome_selectR == "Hospitalisations")
      comparaison_region_plotH(HospsR_comparaison(),input$minimum_dateR)
    else if(input$outcome_selectR == "Réanimations")
      comparaison_region_plotR(ReaR_comparaison(),input$minimum_dateR)
    else if(input$outcome_selectR == "Décès")
      comparaison_region_plotD(DeathsR_comparaison(),input$minimum_dateR)
  })
  
  # le graphique de comparaison des departements
  output$comparaison_dep <- renderPlotly({
    if(input$outcome_selectD == "Infections")
      comparaison_departement_plotC(CasesD_comparaison(),input$minimum_dateD)
    else if(input$outcome_selectD == "Tests")
      comparaison_departement_plotT(TestsD_comparaison(),input$minimum_dateD)
    else if(input$outcome_selectD == "Hospitalisations")
      comparaison_departement_plotH(HospsD_comparaison(),input$minimum_dateD)
    else if(input$outcome_selectD == "Réanimations")
      comparaison_departement_plotR(ReaD_comparaison(),input$minimum_dateD)
    else if(input$outcome_selectD == "Décès")
      comparaison_departement_plotD(DeathsD_comparaison(),input$minimum_dateD)
  })
  
  # la carte de cas par département
  output$mapDepartementC <- renderLeaflet({
    
    palInfectionD <- colorBin( "Oranges", dataDepartementCasTotalRecent()$tauxIncidence, na.color = "#aaff56")

    leaflet(dataDepartementCasTotalRecent()) %>% 
      addTiles() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(~-100,-60,~60,70) %>%
      addLegend("bottomleft", pal = palInfectionD, values = ~dataDepartementCasTotalRecent()$tauxIncidence,labFormat = labelFormat(big.mark = " "),
                title = "<center><em>Taux d'incidence<br/>pour les 30 derniers jours <br/> par 100 000 habitants</em></center>") %>%
      setView(lng = 2.213749, lat = 46.82764, zoom = 5.5) %>%
      addCircleMarkers(layerId = ~nom_departement,weight = 1, radius = ~(P)^(1/3), 
                       fillOpacity = 0.7, color = "#777777",fillColor = ~palInfectionD(tauxIncidence),
                       label = sprintf("<h4>%s</h4><B style = 'color:#008F5A'>Depuis février 2020 :</B><br/>Infections : <B>%s</B> </br>Taux d'incidence moyen* : <B>%s</B><br/>
                       <B style = 'color:#008F5A'> 30 derniers jours : </br></B>Infections : <B>%s</B>
                                       <br/>Taux d'incidence* : <B>%s</B></br></br>(*) sur 30 jours par 100 000 habitants.</br>
                                       <I>Cliquez pour voir</br>l'évolution au cours du temps.</I>", dataDepartementCasTotalRecent()$nom_departement,numberF(dataDepartementCasTotalFinal()$P),
                                       percentF(dataDepartementCasTotalFinal()$tauxIncidence),numberF(dataDepartementCasTotalRecent()$P), percentF(dataDepartementCasTotalRecent()$tauxIncidence)) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(width = "50px",
                                                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = lkp_blue),
                                                   textsize = "15px", direction = "auto"))
  })
  
  # liaison des graphiques avec la carte des cas par département
  observe({
    eventDepC <- input$mapDepartementC_marker_click
    idDepC <- input$mapDepartementC_marker_click$id 
    
    if (is.null(eventDepC))
      idDepC <- "Paris"

    if(input$ageDepC == FALSE){
      output$graphCD <- renderPlot({
        graphiqueDepReg(dataCasDepP(),"dep","Infections","darkorange3",idDepC)
      })
      output$histogramCD <- renderPlot({
        histogramDepReg(dataDepartementCasRecent(),"dep","Infections","darkorange3",idDepC,"nothing")
      })
    }else{
      output$graphCD <- renderPlot({
        graphiqueDepRegFactor(dataCasDepPAges(),"dep","Infections","cl_age",idDepC)
      })
      output$histogramCD <- renderPlot({
        histogramDepReg(dataDepartementCasRecent(),"dep","Infections","darkorange3",idDepC,"cl_age")
      })
      }
  })
  
  # carte des tests par départements 
  output$mapDepartementT <- renderLeaflet({
    
    palTestsD <- colorBin( "Blues", dataDepartementCasTotalRecent()$tauxTests, na.color = "#aaff56")
    
    leaflet(dataDepartementCasTotalRecent()) %>% 
      addTiles() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(~-100,-60,~60,70) %>%
      addLegend("bottomleft", pal = palTestsD, values = ~dataDepartementCasTotalRecent()$tauxTests,labFormat = labelFormat(big.mark = " "),
                title = "<center><em>Taux des tests<br/>pour les 30 derniers jours <br/> par 100 000 habitants</em></center>") %>%
      setView(lng = 2.213749, lat = 46.82764, zoom = 5.5) %>%
      addCircleMarkers(layerId = ~nom_departement,weight = 1, radius = ~(P)^(1/3), 
                       fillOpacity = 0.7, color = "#777777",fillColor = ~palTestsD(tauxTests),
                       label = sprintf("<h4>%s</h4><B style = 'color:#008F5A'>Depuis février 2020 :</B><br/>Tests : <B>%s</B> </br>Taux de tests moyen* : <B>%s</B><br/>
                       <B style = 'color:#008F5A'> 30 derniers jours : </br></B>Tests : <B>%s</B>
                                       <br/>Taux de tests* : <B>%s</B></br></br>(*) sur 30 jours par 100 000 habitants.</br>
                                       <I>Cliquez pour voir</br>l'évolution au cours du temps.</I>", dataDepartementCasTotalRecent()$nom_departement,numberF(dataDepartementCasTotalFinal()$T),
                                       percentF(dataDepartementCasTotalFinal()$tauxTests),numberF(dataDepartementCasTotalRecent()$T), percentF(dataDepartementCasTotalRecent()$tauxTests)) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(width = "50px",
                                                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = lkp_blue),
                                                   textsize = "15px", direction = "auto"))
  })
  # liaisons graphiques de carte tests dep
  observe({
    eventDepT <- input$mapDepartementT_marker_click
    idDepT <- input$mapDepartementT_marker_click$id 
    
    if (is.null(eventDepT))
      idDepT <- "Paris"
    
    if(input$ageDepT == FALSE){
      output$graphTD <- renderPlot({
        graphiqueDepReg(dataTestDepP(),"dep","Tests","steelblue4",idDepT)
      })
      output$histogramTD <- renderPlot({
        histogramDepReg(dataDepartementCasRecent(),"dep","Tests","steelblue4",idDepT,"nothing")
      })
    }else{
      output$graphTD <- renderPlot({
        graphiqueDepRegFactor(dataTestDepPAges(),"dep","Tests","cl_age",idDepT)
      })
      output$histogramTD <- renderPlot({
        histogramDepReg(dataDepartementCasRecent(),"dep","Tests","steelblue4",idDepT,"cl_age")
      })
    }
  })
  
  # carte des hospitalisations par departement
  output$mapDepartementH <- renderLeaflet({
    
    palHospD <- colorBin( "Reds", dataDepartementHospTotalRecent()$tauxHosps, na.color = "#aaff56")
    
    leaflet(dataDepartementHospTotalRecent()) %>% 
      addTiles() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(~-100,-60,~60,70) %>%
      addLegend("bottomleft", pal = palHospD, values = ~dataDepartementHospTotalRecent()$tauxHosps,labFormat = labelFormat(big.mark = " "),
                title = "<center><em>Taux des hospitalisations<br/>pour les 30 derniers jours <br/> par 100 000 habitants</em></center>") %>%
      setView(lng = 2.213749, lat = 46.82764, zoom = 5.5) %>%
      addCircleMarkers(layerId = ~nom_departement,weight = 1, radius = ~(nv_hosp)^(1/2), 
                       fillOpacity = 0.7, color = "#777777",fillColor = ~palHospD(tauxHosps),
                       label = sprintf("<h4>%s</h4><B style = 'color:#008F5A'>Depuis février 2020 :</B><br/>Hospitalisations : <B>%s</B> </br>Taux des hospitalisations moyen* : <B>%s</B><br/>
                       <B style = 'color:#008F5A'> 30 derniers jours : </br></B>Hospitalisations : <B>%s</B>
                                       <br/>Taux des hospitalisations* : <B>%s</B></br></br>(*) sur 30 jours par 100 000 habitants.</br>
                                       <I>Cliquez pour voir</br>l'évolution au cours du temps.</I>", dataDepartementHospTotalRecent()$nom_departement,numberF(dataDepartementHospTotalFinal()$nv_hosp),
                                       percentF(dataDepartementHospTotalFinal()$tauxHosps),numberF(dataDepartementHospTotalRecent()$nv_hosp), percentF(dataDepartementHospTotalRecent()$tauxHosps)) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(width = "50px",
                                                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = lkp_blue),
                                                   textsize = "15px", direction = "auto"))
  })
  
  # liaison entre la carte des hospitalisations et les graphiques
  observe({
    eventDepH <- input$mapDepartementH_marker_click
    idDepH <- input$mapDepartementH_marker_click$id 
    
    if (is.null(eventDepH))
      idDepH <- "Paris"
    
    if(input$sexeDepH == FALSE){
      output$graphHD <- renderPlot({
        graphiqueDepReg(dataHospDepP(),"dep","Hospitalisations","darkred",idDepH)
      })
      output$histogramHD <- renderPlot({
        histogramDepReg(dataDepartementHospRecent(),"dep","Hospitalisations","darkred",idDepH,"nothing")
      })
    }else{
      output$graphHD <- renderPlot({
        graphiqueDepRegFactor(dataHospDepPSexe(),"dep","Hospitalisations","sexe",idDepH)
      })
      output$histogramHD <- renderPlot({
        histogramDepReg(dataDepartementHospRecent(),"dep","Hospitalisations","darkred",idDepH,"sexe")
      })
    }
  })
  
  # cartes des reanimations par departement
  output$mapDepartementR <- renderLeaflet({
    
    palReaD <- colorBin( "Purples", dataDepartementHospTotalRecent()$tauxRea, na.color = "#aaff56")
    
    leaflet(dataDepartementHospTotalRecent()) %>% 
      addTiles() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(~-100,-60,~60,70) %>%
      addLegend("bottomleft", pal = palReaD, values = ~dataDepartementHospTotalRecent()$tauxRea,labFormat = labelFormat(big.mark = " "),
                title = "<center><em>Taux des réanimations<br/>pour les 30 derniers jours <br/> par 100 000 habitants</em></center>") %>%
      setView(lng = 2.213749, lat = 46.82764, zoom = 5.5) %>%
      addCircleMarkers(layerId = ~nom_departement,weight = 1, radius = ~(nv_rea)^(3/4), 
                       fillOpacity = 0.7, color = "#777777",fillColor = ~palReaD(tauxRea),
                       label = sprintf("<h4>%s</h4><B style = 'color:#008F5A'>Depuis février 2020 :</B><br/>Réanimations : <B>%s</B> </br>Taux des réanimations moyen* : <B>%s</B><br/>
                       <B style = 'color:#008F5A'> 30 derniers jours : </br></B>Réanimations : <B>%s</B>
                                       <br/>Taux des réanimations* : <B>%s</B></br></br>(*) sur 30 jours par 100 000 habitants.</br>
                                       <I>Cliquez pour voir</br>l'évolution au cours du temps.</I>", dataDepartementHospTotalRecent()$nom_departement,numberF(dataDepartementHospTotalFinal()$nv_rea),
                                       percentF(dataDepartementHospTotalFinal()$tauxRea),numberF(dataDepartementHospTotalRecent()$nv_rea), percentF(dataDepartementHospTotalRecent()$tauxRea)) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(width = "50px",
                                                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = lkp_blue),
                                                   textsize = "15px", direction = "auto"))
  })
  
  #liaisons graphiques et carte reanimations dep
  observe({
    eventDepR <- input$mapDepartementR_marker_click
    idDepR <- input$mapDepartementR_marker_click$id

    if (is.null(eventDepR))
      idDepR <- "Paris"

    if(input$sexeDepR == FALSE){
      output$graphRD <- renderPlot({
        graphiqueDepReg(dataReaDepP(),"dep","Réanimations","darkorchid4",idDepR)
      })
      output$histogramRD <- renderPlot({
        histogramDepReg(dataDepartementHospRecent(),"dep","Réanimations","darkorchid4",idDepR,"nothing")
      })
    }else{
      output$graphRD <- renderPlot({
        graphiqueDepRegFactor(dataReaDepPSexe(),"dep","Réanimations","sexe",idDepR)
      })
      output$histogramRD <- renderPlot({
        histogramDepReg(dataDepartementHospRecent(),"dep","Réanimations","darkorchid4",idDepR,"sexe")
      })
    }
  })
  
  # la carte des décès par département
  output$mapDepartementD <- renderLeaflet({
    
    palDecesD <- colorBin( "Greys", dataDepartementHospTotalRecent()$tauxDeces, na.color = "#aaff56")
    
    leaflet(dataDepartementHospTotalRecent()) %>% 
      addTiles() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(~-100,-60,~60,70) %>%
      addLegend("bottomleft", pal = palDecesD, values = ~dataDepartementHospTotalRecent()$tauxDeces,labFormat = labelFormat(big.mark = " "),
                title = "<center><em>Taux de décès<br/>pour les 30 derniers jours <br/> par 100 000 habitants</em></center>") %>%
      setView(lng = 2.213749, lat = 46.82764, zoom = 5.5) %>%
      addCircleMarkers(layerId = ~nom_departement,weight = 1, radius = ~nv_dc/2, 
                       fillOpacity = 0.7, color = "#777777",fillColor = ~palDecesD(tauxDeces),
                       label = sprintf("<h4>%s</h4><B style = 'color:#008F5A'>Depuis février 2020 :</B><br/>Décès : <B>%s</B> </br>Taux de décès moyen* : <B>%s</B><br/>
                       <B style = 'color:#008F5A'> 30 derniers jours : </br></B>Décès : <B>%s</B>
                                       <br/>Taux de décès* : <B>%s</B></br></br>(*) sur 30 jours par 100 000 habitants.</br>
                                       <I>Cliquez pour voir</br>l'évolution au cours du temps.</I>", dataDepartementHospTotalRecent()$nom_departement,numberF(dataDepartementHospTotalFinal()$nv_dc),
                                       percentF(dataDepartementHospTotalFinal()$tauxDeces),numberF(dataDepartementHospTotalRecent()$nv_dc), percentF(dataDepartementHospTotalRecent()$tauxDeces)) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(width = "50px",
                                                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = lkp_blue),
                                                   textsize = "15px", direction = "auto"))
  })
  
  #liaison de la carte des décès par dep et les graphiques
  observe({
    eventDepD <- input$mapDepartementD_marker_click
    idDepD <- input$mapDepartementD_marker_click$id
    
    if (is.null(eventDepD))
      idDepD <- "Paris"
    
    if(input$sexeDepD == FALSE){
      output$graphDD <- renderPlot({
        graphiqueDepReg(dataDecesDepP(),"dep","Décès",lkp_grey,idDepD)
      })
      output$histogramDD <- renderPlot({
        histogramDepReg(dataDepartementHospRecent(),"dep","Décès",lkp_grey,idDepD,"nothing")
      })
    }else{
      output$graphDD <- renderPlot({
        graphiqueDepRegFactor(dataDecesDepPSexe(),"dep","Décès","sexe",idDepD)
      })
      output$histogramDD <- renderPlot({
        histogramDepReg(dataDepartementHospRecent(),"dep","Décès",lkp_grey,idDepD,"sexe")
      })
    }
  })
  
  # carte des cas par regions
  output$mapRegionC <- renderLeaflet({
    
    # palInfectionR <- colorBin( "Greens", bins = c(0,700,750,800,850,900,950,1000,2000,4000,6000,8000,10000,12000), na.color = "#aaff56")
    palInfectionR <- colorBin( "Oranges", bins = c(0,600,800,1000,1500,2000,4000,6000,8000,10000,Inf), na.color = "#aaff56")
    
    leaflet(dataRegionCasTotalRecent()) %>% 
      addTiles() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(~-100,-60,~60,70) %>%
      addLegend("bottomleft", pal = palInfectionR, values = ~dataRegionCasTotalRecent()$tauxIncidence,labFormat = labelFormat(big.mark = " "),
                title = "<center><em>Taux d'incidence<br/>pour les 30 derniers jours <br/> par 100 000 habitants</em></center>") %>%
      setView(lng = 2.213749, lat = 46.82764, zoom = 5.5) %>%
      addCircleMarkers(layerId = ~nom_region,weight = 1, radius = ~(P)^(1/3), 
                       fillOpacity = 0.7, color = "#777777",fillColor = ~palInfectionR(tauxIncidence),
                       label = sprintf("<h4>%s</h4><B style = 'color:#008F5A'>Depuis février 2020 :</B><br/>Infections : <B>%s</B> </br>Taux d'incidence moyen* : <B>%s</B><br/>
                       <B style = 'color:#008F5A'> 30 derniers jours : </br></B>Infections : <B>%s</B>
                                       <br/>Taux d'incidence* : <B>%s</B></br></br>(*) sur 30 jours par 100 000 habitants.</br>
                                       <I>Cliquez pour voir</br>l'évolution au cours du temps.</I>", dataRegionCasTotalRecent()$nom_region,numberF(dataRegionCasTotalFinal()$P),
                                       percentF(dataRegionCasTotalFinal()$tauxIncidence),numberF(dataRegionCasTotalRecent()$P), percentF(dataRegionCasTotalRecent()$tauxIncidence)) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(width = "50px",
                                                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = lkp_blue),
                                                   textsize = "15px", direction = "auto"))
  })
  
  # liaison de carte des cas par regions et les graphiques
  observe({
    eventRegC <- input$mapRegionC_marker_click
    idRegC <- input$mapRegionC_marker_click$id 
    
    if (is.null(eventRegC))
      idRegC <- "Île-de-France"
    
    if(length(input$ageSexeC) == 0){
      output$graphCR <- renderPlot({
        graphiqueDepReg(dataCasRegP(),"reg","Infections","darkorange3",idRegC)
      })
      output$histogramCR <- renderPlot({
        histogramDepReg(dataRegionCasRecent(),"reg","Infections","darkorange3",idRegC,"nothing")
      })
    }else if (length(input$ageSexeC) == 1){
      
      if(input$ageSexeC[1] == "Classe d'âge"){
        output$graphCR <- renderPlot({
          graphiqueDepRegFactor(dataCasRegPAges(),"reg","Infections","cl_age",idRegC)
        })
        output$histogramCR <- renderPlot({
          histogramDepReg(dataRegionCasRecent(),"reg","Infections","darkorange3",idRegC,"cl_age")
        })
      }else{
        output$graphCR <- renderPlot({
          graphiqueDepRegFactor(dataCasRegPSexe(),"reg","Infections","sexe",idRegC)
        })
        output$histogramCR <- renderPlot({
          histogramDepReg(dataRegionCasRecentSexe(),"reg","Infections","darkorange3",idRegC,"sexe")
        })
      
    }
    }
    else{
      if(input$ageSexeC[1] == "Classe d'âge"){
        updateCheckboxInput(session, "ageSexeC", value = "Classe d'âge")
      }else{
        updateCheckboxInput(session, "ageSexeC", value = "Sexe")
      }
      }
  })
  
  # carte des tests par regions
  output$mapRegionT <- renderLeaflet({
    
    palTestsR <- colorBin("Blues", dataRegionCasTotalRecent()$tauxTests, na.color = "#aaff56")
    
    leaflet(dataRegionCasTotalRecent()) %>% 
      addTiles() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(~-100,-60,~60,70) %>%
      addLegend("bottomleft", pal = palTestsR, values = ~dataRegionCasTotalRecent()$tauxTests,labFormat = labelFormat(big.mark = " "),
                title = "<center><em>Taux des tests<br/>pour les 30 derniers jours <br/> par 100 000 habitants</em></center>") %>%
      setView(lng = 2.213749, lat = 46.82764, zoom = 5.5) %>%
      addCircleMarkers(layerId = ~nom_region,weight = 1, radius = ~(P)^(1/3), 
                       fillOpacity = 0.7, color = "#777777",fillColor = ~palTestsR(tauxTests),
                       label = sprintf("<h4>%s</h4><B style = 'color:#008F5A'>Depuis février 2020 :</B><br/>Tests : <B>%s</B> </br>Taux de tests moyen* : <B>%s</B><br/>
                       <B style = 'color:#008F5A'> 30 derniers jours : </br></B>Tests : <B>%s</B>
                                       <br/>Taux de tests* : <B>%s</B></br></br>(*) sur 30 jours par 100 000 habitants.</br>
                                       <I>Cliquez pour voir</br>l'évolution au cours du temps.</I>", dataRegionCasTotalRecent()$nom_region,numberF(dataRegionCasTotalFinal()$T),
                                       percentF(dataRegionCasTotalFinal()$tauxTests),numberF(dataRegionCasTotalRecent()$T), percentF(dataRegionCasTotalRecent()$tauxTests)) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(width = "50px",
                                                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = lkp_blue),
                                                   textsize = "15px", direction = "auto"))
  })
  
  # liaison de carte de tests par regions et les graphiques
  observe({
    eventRegT <- input$mapRegionT_marker_click
    idRegT <- input$mapRegionT_marker_click$id 
    
    if (is.null(eventRegT))
      idRegT <- "Île-de-France"
    
    if(length(input$ageSexeT) == 0){
      output$graphTR <- renderPlot({
        graphiqueDepReg(dataTestRegP(),"reg","Tests","steelblue4",idRegT)
      })
      output$histogramTR <- renderPlot({
        histogramDepReg(dataRegionCasRecent(),"reg","Tests","steelblue4",idRegT,"nothing")
      })
    }else if (length(input$ageSexeT) == 1){
      
      if(input$ageSexeT[1] == "Classe d'âge"){
        output$graphTR <- renderPlot({
          graphiqueDepRegFactor(dataTestRegPAges(),"reg","Tests","cl_age",idRegT)
        })
        output$histogramTR <- renderPlot({
          histogramDepReg(dataRegionCasRecent(),"reg","Tests","steelblue4",idRegT,"cl_age")
        })
      }else{
        output$graphTR <- renderPlot({
          graphiqueDepRegFactor(dataTestRegPSexe(),"reg","Tests","sexe",idRegT)
        })
        output$histogramTR <- renderPlot({
          histogramDepReg(dataRegionTestRecentSexe(),"reg","Tests","steelblue4",idRegT,"sexe")
        })
        
      }
    }
    else{
      if(input$ageSexeT[1] == "Classe d'âge"){
        updateCheckboxInput(session, "ageSexeT", value = "Classe d'âge")
      }else{
        updateCheckboxInput(session, "ageSexeT", value = "Sexe")
      }
    }
  })
  
  # carte des hospitalisations par region
  output$mapRegionH <- renderLeaflet({
    
    palHospR <- colorBin( "Reds", bins = c(0,0.4,0.8,1,1.2,1.4,2,10,30,Inf), na.color = "#aaff56")
    
    leaflet(dataRegionHospTotalRecent()) %>% 
      addTiles() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(~-100,-60,~60,70) %>%
      addLegend("bottomleft", pal = palHospR, values = ~dataRegionHospTotalRecent()$tauxHosps,labFormat = labelFormat(big.mark = " "),
                title = "<center><em>Taux des hospitalisations<br/>pour les 30 derniers jours <br/> par 100 000 habitants</em></center>") %>%
      setView(lng = 2.213749, lat = 46.82764, zoom = 5.5) %>%
      addCircleMarkers(layerId = ~nom_region,weight = 1, radius = ~(nv_hosp)^(1/2), 
                       fillOpacity = 0.7, color = "#777777",fillColor = ~palHospR(tauxHosps),
                       label = sprintf("<h4>%s</h4><B style = 'color:#008F5A'>Depuis février 2020 :</B><br/>Hospitalisations : <B>%s</B> </br>Taux des hospitalisations moyen* : <B>%s</B><br/>
                       <B style = 'color:#008F5A'> 30 derniers jours : </br></B>Hospitalisations : <B>%s</B>
                                       <br/>Taux des hospitalisations* : <B>%s</B></br></br>(*) sur 30 jours par 100 000 habitants.</br>
                                       <I>Cliquez pour voir</br>l'évolution au cours du temps.</I>", dataRegionHospTotalRecent()$nom_region,numberF(dataRegionHospTotalFinal()$nv_hosp),
                                       percentF(dataRegionHospTotalFinal()$tauxHosps),numberF(dataRegionHospTotalRecent()$nv_hosp), percentF(dataRegionHospTotalRecent()$tauxHosps)) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(width = "50px",
                                                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = lkp_blue),
                                                   textsize = "15px", direction = "auto"))
  })
  
  # liaison carte hosps par region avec les graphiques
  observe({
    eventRegH <- input$mapRegionH_marker_click
    idRegH <- input$mapRegionH_marker_click$id

    if (is.null(eventRegH))
      idRegH <- "Île-de-France"

    if(input$ageRegH == FALSE){
      output$graphHR <- renderPlot({
        graphiqueDepReg(dataHospRegP(),"reg","Hospitalisations","darkred",idRegH)
      })
      output$histogramHR <- renderPlot({
        histogramDepReg(dataRegionHospRecent(),"reg","Hospitalisations","darkred",idRegH,"nothing")
      })
    }else{
      output$graphHR <- renderPlot({
        graphiqueDepRegFactor(dataHospRegPAges(),"reg","Hospitalisations","cl_age",idRegH)
      })
      output$histogramHR <- renderPlot({
        histogramDepReg(dataRegionHospRecent(),"reg","Hospitalisations","darkred",idRegH,"cl_age")
      })
    }
  })
  
  # carte des reanimations par region
  output$mapRegionR <- renderLeaflet({
    
    palReaR <- colorBin( "Purples", bins = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,3,5,Inf), na.color = "#aaff56")
    
    leaflet(dataRegionHospTotalRecent()) %>% 
      addTiles() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(~-100,-60,~60,70) %>%
      addLegend("bottomleft", pal = palReaR, values = ~dataRegionHospTotalRecent()$tauxRea,labFormat = labelFormat(big.mark = " "),
                title = "<center><em>Taux des réanimations<br/>pour les 30 derniers jours <br/> par 100 000 habitants</em></center>") %>%
      setView(lng = 2.213749, lat = 46.82764, zoom = 5.5) %>%
      addCircleMarkers(layerId = ~nom_region,weight = 1, radius = ~(nv_rea)^(1/2), 
                       fillOpacity = 0.7, color = "#777777",fillColor = ~palReaR(tauxRea),
                       label = sprintf("<h4>%s</h4><B style = 'color:#008F5A'>Depuis février 2020 :</B><br/>Réanimations : <B>%s</B> </br>Taux des réanimations moyen* : <B>%s</B><br/>
                       <B style = 'color:#008F5A'> 30 derniers jours : </br></B>Réanimations : <B>%s</B>
                                       <br/>Taux des réanimations* : <B>%s</B></br></br>(*) sur 30 jours par 100 000 habitants.</br>
                                       <I>Cliquez pour voir</br>l'évolution au cours du temps.</I>", dataRegionHospTotalRecent()$nom_region,numberF(dataRegionHospTotalFinal()$nv_rea),
                                       percentF(dataRegionHospTotalFinal()$tauxRea),numberF(dataRegionHospTotalRecent()$nv_rea), percentF(dataRegionHospTotalRecent()$tauxRea)) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(width = "50px",
                                                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = lkp_blue),
                                                   textsize = "15px", direction = "auto"))
  })
  
  # liaiason carte rea par region et graphiques
  observe({
    eventRegR <- input$mapRegionR_marker_click
    idRegR <- input$mapRegionR_marker_click$id
    
    if (is.null(eventRegR))
      idRegR <- "Île-de-France"
    
    if(input$ageRegR == FALSE){
      output$graphRR <- renderPlot({
        graphiqueDepReg(dataReaRegP(),"reg","Réanimations","darkorchid4",idRegR)
      })
      output$histogramRR <- renderPlot({
        histogramDepReg(dataRegionHospRecent(),"reg","Réanimations","darkorchid4",idRegR,"nothing")
      })
    }else{
      output$graphRR <- renderPlot({
        graphiqueDepRegFactor(dataReaRegPAges(),"reg","Réanimations","cl_age",idRegR)
      })
      output$histogramRR <- renderPlot({
        histogramDepReg(dataRegionHospRecent(),"reg","Réanimations","darkorchid4",idRegR,"cl_age")
      })
    }
  })
  
  # carte de décès par region
  output$mapRegionD <- renderLeaflet({
    
    palDecesR <- colorBin( "Greys", bins = c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.5,3,5,7,Inf), na.color = "#aaff56")
    
    leaflet(dataRegionHospTotalRecent()) %>% 
      addTiles() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(~-100,-60,~60,70) %>%
      addLegend("bottomleft", pal = palDecesR, values = ~dataRegionHospTotalRecent()$tauxDeces,labFormat = labelFormat(big.mark = " "),
                title = "<center><em>Taux de décès<br/>pour les 30 derniers jours <br/> par 100 000 habitants</em></center>") %>%
      setView(lng = 2.213749, lat = 46.82764, zoom = 5.5) %>%
      addCircleMarkers(layerId = ~nom_region,weight = 1, radius = ~nv_dc/3, 
                       fillOpacity = 0.7, color = "#777777",fillColor = ~palDecesR(tauxDeces),
                       label = sprintf("<h4>%s</h4><B style = 'color:#008F5A'>Depuis février 2020 :</B><br/>Décès : <B>%s</B> </br>Taux de décès moyen* : <B>%s</B><br/>
                       <B style = 'color:#008F5A'> 30 derniers jours : </br></B>Décès : <B>%s</B>
                                       <br/>Taux de décès* : <B>%s</B></br></br>(*) sur 30 jours par 100 000 habitants.</br>
                                       <I>Cliquez pour voir</br>l'évolution au cours du temps.</I>", dataRegionHospTotalRecent()$nom_region,numberF(dataRegionHospTotalFinal()$nv_dc),
                                       percentF(dataRegionHospTotalFinal()$tauxDeces),numberF(dataRegionHospTotalRecent()$nv_dc), percentF(dataRegionHospTotalRecent()$tauxDeces)) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(width = "50px",
                                                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = lkp_blue),
                                                   textsize = "15px", direction = "auto"))
  })
  
  # liaison de carte décès par région et les graphiques
  observe({
    eventRegD <- input$mapRegionD_marker_click
    idRegD <- input$mapRegionD_marker_click$id
    
    if (is.null(eventRegD))
      idRegD <- "Île-de-France"
    
    if(input$ageRegD == FALSE){
      output$graphDR <- renderPlot({
        graphiqueDepReg(dataDecesRegP(),"reg","Décès",lkp_grey,idRegD)
      })
      output$histogramDR <- renderPlot({
        histogramDepReg(dataRegionHospRecent(),"reg","Décès",lkp_grey,idRegD,"nothing")
      })
    }else{
      output$graphDR <- renderPlot({
        graphiqueDepRegFactor(dataDecesRegPAges(),"reg","Décès","cl_age",idRegD)
      })
      output$histogramDR <- renderPlot({
        histogramDepReg(dataRegionHospRecent(),"reg","Décès",lkp_grey,idRegD,"cl_age")
      })
    }
  })

  # ajout des cercles des infections à la carte du monde 
  observe({
    palInfection <- colorBin( "Oranges", bins=c(0,30,60,80,100,200,400,800,1000,1500,2000,4500), na.color = "#aaff56")
    
    leafletProxy("map",data = CasesL()) %>%
      clearMarkers() %>%
      clearShapes() %>% 
      addCircleMarkers(layerId = ~Country,weight = 1, radius = ~(Cases)^(1/6), 
                       fillOpacity = 0.7, color = "#777777", group = "Infections",fillColor = ~palInfection(tauxInfection),
                       label = sprintf("<h4>%s</h4><B style = 'color:#008F5A'>Depuis février 2020 :</B><br/>Infections : <B>%s</B> </br>Taux d'incidence moyen* : <B>%s</B><br/>
                       <B style = 'color:#008F5A'> 30 derniers jours : </br></B>Infections : <B>%s</B>
                                       <br/>Taux d'incidence* : <B>%s</B></br></br>(*) sur 30 jours par 100 000 habitants.</br>
                                       <I>Cliquez pour voir</br>l'évolution au cours du temps.</I>", CasesL()$Pays,numberF(CasesL()$Cases),percentF(CasesL()$cases100milles),
                                       numberF(CasesL()$newCases), percentF(CasesL()$tauxInfection)) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(width = "50px",
                                                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = lkp_blue),
                                                   textsize = "15px", direction = "auto"))
    
  })
  
  # ajout des cercles des décès à la carte du monde 
  observe({
    
    palDeces <- colorBin("Greys", bins=c(0,1,2,3,4,5,10,15,20,30,50,60,80), na.color = "#aaff56")
    
    leafletProxy("map",data = DeathsL()) %>%
      addCircleMarkers(layerId = ~Code,weight = 1, radius = ~(Deaths)^(1/5), 
                       fillOpacity = 0.7, color = "#777777", group = "Décès",fillColor = ~palDeces(tauxDeces),
                       label = sprintf("<h4>%s</h4><B style = 'color:#008F5A'>Depuis février 2020 :</B><br/>Décès : <B>%s</B> </br>Taux de décès moyen* : <B>%s</B><br/>
                       <B style = 'color:#008F5A'> 30 derniers jours : </br></B>Décès : <B>%s</B>
                                       <br/>Taux de décès* : <B>%s</B></br></br>(*) sur 30 jours par 100 000 habitants.</br>
                                       <I>Cliquez pour voir</br>l'évolution au cours du temps.</I>", DeathsL()$Pays,numberF(DeathsL()$Deaths),percentF(DeathsL()$deaths100milles),
                                       numberF(DeathsL()$newDeaths), percentF(DeathsL()$tauxDeces)) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(width = "50px",
                                                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = lkp_blue),
                                                   textsize = "15px", direction = "auto"))
  })
  
  # fonction qui gère l'affichage de popup des deces du monde
  showPopup <- function(id,lat,lng){
    folder <- tempfile()
    dir.create(folder)
    folder1 <- tempfile()
    dir.create(folder1)
    gr <- graphique(id)
    h <- histogram(id)
    svg(filename= paste(folder,"plot.svg", sep = "/"), 
        width = 600 * 0.011, height = 450 * 0.01)
    grid.arrange(gr,h)
    dev.off()
    content <- paste(readLines(paste(folder,"plot.svg",sep="/")),collapse = "")
    leafletProxy("map")  %>%
      addPopups(lng, lat,content,layerId = id,group="Infections",options = popupOptions(minWidth = 640,
                                                                                        max_width = 640))
  }
  
  # fonction qui gère l'affichage de popup des infections du monde
  showPopupD <- function(id,lat,lng){
    folder <- tempfile()
    dir.create(folder)
    gr <- graphiqueD(id)
    h <- histogramD(id)
    svg(filename= paste(folder,"plot.svg", sep = "/"), 
        width = 600 * 0.011, height = 450 * 0.01)
    grid.arrange(gr,h)
    dev.off()
    content <- paste(readLines(paste(folder,"plot.svg",sep="/")), collapse = "")
    leafletProxy("map")  %>% 
      addPopups(lng, lat,content,layerId = id,group="Deces",options = popupOptions(maxWidth = 640,
                                                                                   minWidth = 640))
    
  }
  # liaison de popups avec la carte du monde
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    id <- input$map_marker_click$id 
    if (is.null(event))
      return()
    
    print(id)
    
    if(id %in% casesP()$Country){
      isolate({
        showPopup(event$id, event$lat, event$lng)
        
      })
    }
    else if (id %in% deathsP()$Code){
      isolate({
        showPopupD(event$id, event$lat, event$lng)
        
      })
    }
  })
}

# la fonction qui lance l'application
shinyApp(ui, server)

