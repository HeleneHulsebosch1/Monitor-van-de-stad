
### Libraries inladen ###

library(tidyverse)
library(lubridate)
library(sqldf)
library(RColorBrewer)
library(shiny)
library(shinydashboard)




### Data inladen en filteren op gemeente Tilburg ###

myurl <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQAAu_7zDMuize0vbj8vVvFc0parcLy2MIQtkPHmZNrDyfus3ErnlnDQQR1YNVWdf8GO04Qfzyvk_nM/pub?gid=176908832&single=true&output=csv"
dataset <- read.csv(url(myurl))
dataset <- dataset %>% rename("Woonachtig" = Bent.u.woonachtig.in.de.gemeente.Tilburg.,
                              "Geslacht" = Wat.is.uw.geslacht.,
                              "Leeftijd" = In.welke.leeftijdscategorie.bevindt.u.zich.,
                              "Wijk" = In.welke.woonwijk.woont.u.,
                              "Milieu" = Hoe.staat.u.tegenover.milieuvervuiling.,
                              "Afval" = Wat.voor.soorten.afval.scheidt.u.,
                              "Klimaat" = De.term..klimaatverandering..wordt.vaak.gebruikt.om.te.verwijzen.naar.het.idee.dat.de.gemiddelde.temperatuur.op.de.aarde.stijgt..In.hoeverre.is.dit.volgens.u.een.probleem.,
                              "Invloed" = In.hoeverre.denkt.u.dat.de.mens.hier.invloed.op.heeft.,
                              "Gedrag" = Denkt.u.dat.een.verandering.in.menselijk.gedrag.het.opwarmen.van.de.aarde.kan.verminderen.,
                              "Leefstijl" = Hoe.bereid.bent.u.om.uw.levensstijl.te.veranderen...te.verduurzamen.om.milieuvervuiling.tegen.te.gaan.,
                              "Temperatuur" = Een.kaart.waarop.u.de.temperatuurverschillen.binnen.de.stad.kunt.zien.,                                                                                                    
                              "Luchtvochtigheid" = Een.kaart.waarop.u.de.luchtvochtigheid.van.verschillende.wijken.kunt.zien.,                                                                                                 
                              "Luchtkwaliteit" = Een.kaart.waarop.u.de.luchtkwaliteit.van.verschillende.wijken.kunt.zien.,                                                                                                 
                              "Verstening" = Een.kaart.waar.de.mate.van.verstening.weergegeven.wordt.,                                                                                                   
                              "Vergroening" = Een.kaart.waar.de.mate.van.vergroening.weergegeven.wordt.,                                                                                                                  
                              "Vergrijzing" = Een.kaart.waarop.u.de.vergrijzing.per.wijk.kunt.zien.,                                                                                                                  
                              "Inkomen" = Een.kaart.waarop.u.het.gemiddelde.inkomen.per.wijk.kunt.zien.,                                                                                                              
                              "Drukte" = Een.kaart.waarop.u.kunt.zien.hoe.druk.het.op.verschillende.locaties.in.Tilburg.is.,                                                                                         
                              "Levensverwachting" = Een.kaart.waarop.u.de.levensverwachting.per.wijk.kunt.zien.,                                                                                                                
                              "Parkeerdrukte" = Een.kaart.waarop.u.de.parkeerdrukte.kunt.zien.,                                                                                                                             
                              "Winkelleegstand" = Een.kaart.waarop.u.de.winkelleegstand.kunt.zien.,
                              "Geluidsoverlast" = Een.kaart.waarop.u.geluidsoverlast.te.zien.is.,
                              "Zonnepanelen" = Een.kaart.waarop.u.alle.zonnepanelen.kunt.zien
                              ) %>%
                       filter(Woonachtig == "Ja")



### Vectoren voor controls ###

GeslachtV <- c("Man", "Vrouw", "Anders")
LeeftijdV <- c("< 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74","> 74")
WijkV <- c("Centrum", "Oud-Noord", "Oud-Zuid", "Noord", "Zuid", "West", "Oost", "Reeshof", "Berkel-Enschot", "Udenhout")




### UI (input en output) ###

ui <- dashboardPage(
    
    dashboardHeader(title = "Dashboard Enquête"),
    
    dashboardSidebar({
        sidebarMenu(
            menuItem("Milieu- en klimaatvragen", tabName = "Milieu", icon = icon("globe-americas")),
            menuItem("Kaartvragen", tabName = "Kaarten", icon = icon("map")),
            menuItem("Bedieningspaneel", icon = icon("filter"),
            menuItem("Geslacht", checkboxInput("InputGeslachtAll", "Selecteer Alles / Niks", TRUE), checkboxGroupInput("InputGeslacht", NULL, GeslachtV), icon = icon("venus-mars")),
            menuItem("Leeftijd", checkboxInput("InputLeeftijdAll", "Selecteer Alles / Niks", TRUE), checkboxGroupInput("InputLeeftijd", NULL, LeeftijdV), icon = icon("birthday-cake")),
            menuItem("Wijk", checkboxInput("InputWijkAll", "Selecteer Alles / Niks", TRUE), checkboxGroupInput("InputWijk", NULL, WijkV), icon = icon("home"))
            )
        )
    }),
    
    dashboardBody(
        tabItems(
            tabItem("Milieu",
                fluidRow(
                    valueBoxOutput("TotaalEnqueteM"),
                    valueBoxOutput("ProcentEnqueteM"),
                    valueBoxOutput("TijdEnqueteM")
                ),
                
                fluidRow(
                    box(
                        title = "Hoe staan Tilburgers tegenover milieuvervuiling?",
                        plotOutput("plotMilieu", height = 350),
                        width = 6,
                        status = "info",
                        collapsible = TRUE
                    ),
                    
                    
                    #door verschillende comma gescheiden data in dezelfde regel werkt deze tabel niet. In overleg met Rob deze weggehaald. 
                    #box(
                     #   title = "Wat voor soorten afval scheiden Tilburgers?",
                     #  plotOutput("plotAfval", height = 350),
                     # width = 6,
                     #status = "info",
                     #collapsible = TRUE
                    #),
                    box(
                        title = "Vinden Tilburgers klimaatverandering een probleem?",
                        plotOutput("plotKlimaat", height = 350),
                        width = 6,
                        status = "info",
                        collapsible = TRUE
                    ),
                    box(
                        title = "Hoeveel invloed vinden Tilburgers dat de mens op klimaatverandering heeft?",
                        plotOutput("plotInvloed", height = 350),
                        width = 6,
                        status = "info",
                        collapsible = TRUE
                    ),
                    box(
                        title = "Denken Tilburgers dat verandering van menselijk gedrag het opwarmen kan verminderen?",
                        plotOutput("plotGedrag", height = 350),
                        width = 6,
                        status = "info",
                        collapsible = TRUE
                    ),
                    box(
                        title = "Zijn Tilburgers bereid hun leefstijl te veranderen / te verduurzamen?",
                        plotOutput("plotLeefstijl", height = 350),
                        width = 6,
                        status = "info",
                        collapsible = TRUE
                    )
                )
            ),
            
            tabItem("Kaarten",
                fluidRow(
                    valueBoxOutput("TotaalEnqueteK"),
                    valueBoxOutput("ProcentEnqueteK"),
                    valueBoxOutput("TijdEnqueteK")
                ),
                
                fluidRow(
                    box(title = "In welke soort kaart is men geïnteresseerd?",
                        plotOutput("plotkeuze"),
                        height = 500,
                        width = 6,
                        status = "info",
                        collapsible = TRUE,
                        #mode(dataset$Temperatuur)
                        ), 
                    
                    box(title = "De keuze welke kaart wordt getoond",
                        varSelectInput("kaartkeuze", "Kaartkeuzes:", dataset[12:21]), 
                        plotOutput("keuze"),
                        height = 500,
                        width = 6, 
                        status = "info",
                        collapsible = TRUE,
                        ), 
                )
                
        
                )
            )
        )
    )





### Code die de inputs verwerkt tot outputs en naar de UI stuurt ###

server <- function(input, output, session) {
    
    ### Code voor een refresher ###
    autoUpdate <- reactiveTimer(1000)
    
    ### Code die de dataset filterd op basis van het Bedieningspaneel
    Selectie <- reactive({
        selectieDataset <- dataset %>% filter(Geslacht %in% input$InputGeslacht, 
                                              Leeftijd %in% input$InputLeeftijd,
                                              Wijk %in% input$InputWijk
                                              )
    })
    
    ### Code voor de selecteer alles / niks knoppen ###
    observe({
        updateCheckboxGroupInput(session, 'InputGeslacht', choices = GeslachtV, selected = if (input$InputGeslachtAll) GeslachtV)
        updateCheckboxGroupInput(session, 'InputLeeftijd', choices = LeeftijdV, selected = if (input$InputLeeftijdAll) LeeftijdV)
        updateCheckboxGroupInput(session, 'InputWijk', choices = WijkV, selected = if (input$InputWijkAll) WijkV)
    })
    
    ### Code voor de totaal enquete box ###
    TotaalEnquete <- reactive({
        valueBox(
            value = nrow(dataset),
            subtitle = "Totaal beantwoorde enquêtes",
            icon = icon("poll")
        )
    })
    output$TotaalEnqueteM <- renderValueBox({
        TotaalEnquete()
    })
    output$TotaalEnqueteK <- renderValueBox({
        TotaalEnquete()
    })
    
    ### Code voor de procent geselecteerd box ###
    ProcentEnquete <- reactive({
        kommaEnquete <- round(nrow(Selectie()) / nrow(dataset) * 100, digits = 1)
        percentageEnquete <- paste(kommaEnquete, "%", sep="")
        valueBox(
            value = percentageEnquete,
            subtitle = "Selectie percentage",
            icon = icon("percent")
        )
    })
    output$ProcentEnqueteM <- renderValueBox({
        ProcentEnquete()
    })
    output$ProcentEnqueteK <- renderValueBox({
        ProcentEnquete()
    })
    
    ### Code voor tijd box ###
    TijdEnquete <- reactive({
        autoUpdate()
        datum <- difftime(Sys.time(), max(as.POSIXct(dataset$Tijdstempel, format="%d-%m-%Y %H:%M:%S")))
        dagen    <- paste(datum %/% ddays(1), "d", sep="")
        uren   <- paste(datum %/% dhours(1) %% 24, "u", sep="")
        minuten <- paste(datum %/% dminutes(1) %% 60, "m", sep="")
        seconden <- paste(datum %/% dseconds(1) %% 60, "s", sep="")
        datum <- paste(dagen, uren, minuten, seconden, sep=" ")
        valueBox(
            value = datum,
            subtitle = "Tijd sinds laatste antwoord",
            icon = icon("clock")
        )
    })
    output$TijdEnqueteM <- renderValueBox({
        TijdEnquete()
    })
    output$TijdEnqueteK <- renderValueBox({
        TijdEnquete()
    })
    
    ### Code voor de grafieken milieu- en klimaatvragen ###
    ### Milieu ###
    
    output$plotMilieu <- renderPlot({
        Selectie() %>% ggplot(aes(x=Milieu, fill = Milieu)) + 
                       geom_bar(color = "#FFFFFF") + 
                       ylab("Aantal keer beantwoord") +
                       xlab("") + 
                       theme(axis.title=element_text(size = 15), 
                           axis.text.x=element_blank(), 
                           axis.text=element_text(size = 15),
                           legend.text=element_text(size=15), 
                           legend.title = element_blank(), 
                           legend.position = c(.99, .98), 
                           legend.justification = c("right", "top"), 
                           legend.box.just = "right") +
                       scale_x_discrete(limits = c("Doet mij helemaal niets", "Ik denk er niet over na", "Ik denk er soms over na", "Ik denk er veel over na", "Ik maak me zorgen", "Weet ik niet / geen mening")) + 
                       scale_y_continuous(breaks = seq(0, 1000, by = 5)) + 
                       scale_fill_brewer(palette = "PuBuGn")
                        
    })
    
    
    ### Klimaat ###
    
    output$plotKlimaat <- renderPlot({
        Selectie() %>% mutate(Klimaat = fct_relevel(Klimaat, "Geen probleem", "Klein probleem", "Enigzins een probleem", "Redelijk groot probleem", "Heel groot probleem", "Weet ik niet / geen mening")) %>%
                    ggplot(aes(x=Klimaat, fill = Klimaat)) + 
                    geom_bar(color = "#FFFFFF") + 
                    ylab("Aantal keer beantwoord") +
                    xlab("") +
                    labs(fill = "Legenda")+
                    theme(axis.title=element_text(size = 15), 
                        axis.text.x=element_blank(), 
                        axis.text=element_text(size = 15),
                        legend.text=element_text(size=15), 
                        legend.position = c(.99, .98), 
                        legend.justification = c("right", "top"), 
                        legend.box.just = "right") +
                    scale_x_discrete(limits = c("Geen probleem", "Klein probleem", "Enigzins een probleem", "Redelijk groot probleem", "Heel groot probleem", "Weet ik niet / geen mening")) + 
                    scale_y_continuous(breaks = seq(0, 1000, by = 5)) + 
                    scale_fill_brewer(palette = "PuBuGn")
    })

    ### Gedrag ### 
    
    output$plotGedrag <- renderPlot({
        Selectie() %>% 
            mutate(Gedrag = fct_relevel(Gedrag, "Ja", "Misschien", "Nee", "Weet ik niet / geen mening", "Anders")) %>%
            ggplot(aes(x=Gedrag, fill = Gedrag)) + 
            geom_bar(color = "#FFFFFF") + 
            ylab("Aantal keer beantwoord") +
            xlab("") + 
            theme(axis.title=element_text(size = 15), 
                  axis.text.x=element_blank(), 
                  axis.text=element_text(size = 15),
                  legend.text=element_text(size=15), 
                  legend.title = element_blank(), 
                  legend.position = c(.99, .98), 
                  legend.justification = c("right", "top"), 
                  legend.box.just = "right") +
            scale_x_discrete(limits = c("Ja", "Misschien", "Nee", "Weet ik niet / geen mening", "Anders")) + 
            scale_y_continuous(breaks = seq(0, 1000, by = 5)) + 
            scale_fill_brewer(palette = "PuBuGn")
        
    })
    
    ### Afval ###
    
    #output$plotAfval <- renderPlot({
       # Selectie() %>% 
          #  separate(dataset, col = Afval, c("Papier en karton", "PMD", "GFT", "Grof huishoudelijk afval", "Textiel", "Elektrische apparaten", "Ik scheid geen afval")) %>%
            
        
          #  mutate(Afval = fct_relevel(Afval, "Papier en karton", "PMD", "GFT", "Grof huishoudelijk afval", "Textiel", "Elektrische apparaten", "Ik scheid geen afval")) %>%
          #  ggplot(aes(x=Afval, fill = Afval)) + 
          #  geom_bar(color = "#FFFFFF") + 
          #  ylab("Aantal keer beantwoord") +
          #  xlab("") + 
          #  theme(axis.title=element_text(size = 15), 
          #        axis.text.x=element_blank(), 
          #        axis.text=element_text(size = 15),
          #        legend.text=element_text(size=15), 
          #        legend.title = element_blank(), 
          #        legend.position = c(.99, .98), 
          #        legend.justification = c("right", "top"), 
          #        legend.box.just = "right") +
          #  scale_x_discrete(limits = c("Papier en karton", "PMD", "GFT", "Grof huishoudelijk afval", "Textiel", "Elektrische apparaten", "Ik scheid geen afval")) + 
          #  scale_y_continuous(breaks = seq(0, 1000, by = 5)) + 
          #  scale_fill_brewer(palette = "PuBuGn")
    
    #})

    
    
    ### Invloed ###
    
    output$plotInvloed <- renderPlot({
        Selectie() %>% 
            mutate(Invloed = fct_relevel(Invloed, "Volledige invloed", "Grote invloed", "Redelijke invloed", "Weinig invloed", "Geen invloed", "Weet ik niet / geen mening")) %>%
            ggplot(aes(x=Invloed, fill = Invloed)) + 
            geom_bar(color = "#FFFFFF") + 
            ylab("Aantal keer beantwoord") +
            xlab("") + 
            theme(axis.title=element_text(size = 15), 
                  axis.text.x=element_blank(), 
                  axis.text=element_text(size = 15),
                  legend.text=element_text(size=15), 
                  legend.title = element_blank(), 
                  legend.position = c(.99, .98), 
                  legend.justification = c("right", "top"), 
                  legend.box.just = "right") +
            scale_x_discrete(limits = c("Volledige invloed", "Grote invloed", "Redelijke invloed", "Weinig invloed", "Geen invloed", "Weet ik niet / geen mening")) + 
            scale_y_continuous(breaks = seq(0, 1000, by = 5)) + 
            scale_fill_brewer(palette = "PuBuGn")
        
    })
    
    
    ### Leefstijl ###
    
    output$plotLeefstijl <- renderPlot({
        Selectie() %>% 
            mutate(Leefstijl = fct_relevel(Leefstijl, "Zeer bereid", "Best wel bereid", "Neutraal", "Niet zo bereid", "Helemaal niet bereid", "Weet ik niet / geen mening")) %>%
            ggplot(aes(x=Leefstijl, fill = Leefstijl)) + 
            geom_bar(color = "#FFFFFF") + 
            ylab("Aantal keer beantwoord") +
            xlab("") + 
            theme(axis.title=element_text(size = 15), 
                  axis.text.x=element_blank(), 
                  axis.text=element_text(size = 15),
                  legend.text=element_text(size=15), 
                  legend.title = element_blank(), 
                  legend.position = c(.99, .98), 
                  legend.justification = c("right", "top"), 
                  legend.box.just = "right") +
            scale_x_discrete(limits = c("Zeer bereid", "Best wel bereid", "Neutraal", "Niet zo bereid", "Helemaal niet bereid", "Weet ik niet / geen mening")) + 
            scale_y_continuous(breaks = seq(0, 1000, by = 5)) + 
            scale_fill_brewer(palette = "PuBuGn")
        
    })
    

    ### Code voor grafiek kaartvragen ### 


    output$plotkeuze <-  renderPlot({
       Selectie() %>%
        ggplot(aes(!!input$kaartkeuze)) +  
                geom_bar(color = "#FFFFFF", fill = "skyblue3") +
                   ggtitle(input$kaartkeuze) +
                   ylab("Aantal keer beantwoord") +
                   xlab("Interesse niveau ") + 
                   theme(plot.title = element_text(size = 17),
                         axis.title=element_text(size = 15), 
                         axis.text.x=element_text(""), 
                         axis.text=element_text(size = 12),
                         legend.text=element_text(size=15), 
                         legend.title = element_blank(), 
                         legend.position = c(.99, .98), 
                         legend.justification = c("right", "top"), 
                         legend.box.just = "right") +
            scale_x_discrete(limits = c("Zeer oninteressant", "Oninteressant", "Neutraal", "Interessant", "Zeer interessant")) +
            scale_y_continuous(breaks = seq(0, 1000, by = 5)) + 
            scale_fill_brewer(palette = "PuBuGn") +
            geom_text(
                aes(label=stat(prop)*100), 
                stat = "count", 
                format_string='{:.1f}%')
        
        })


}




### Code die de app initialiseert en opent ###

shinyApp(ui, server)