library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)
library(stringr)

ui <- shinyUI(fluidPage(
  
  titlePanel("THE BIOMEDICAL RESEARCH PROJECT"),
  br(),
  br(),
  sidebarLayout(position = "left", 
                sidebarPanel(width = 2,
                             checkboxGroupInput("species", label = h4("Select Species"),
                                                choices = list("Mouse"="Mouse","Rat"="Rat","Human"="Human","Non-human primate"="Non-human primate"),
                                                selected = "Mouse"),
                             checkboxGroupInput("drug", label = h4("Select Drugs"),
                                                choices = list("Rosiglitazone/Avandia" = "Rosiglitazone/Avandia",
                                                               "Troglitazone/Rezulin" = "Troglitazone/Rezulin"),
                                                selected = "Rosiglitazone/Avandia"),
                             selectInput("altunit", label = h4("ALT Unit"), 
                                         choices = list("U/L" = "U/L", "IUA" = "IUA", "counts" = "counts"), 
                                         selected = "U/L"),
                             br(),
                             actionButton("logdose","(Un)Logarithmic Scale")
                ),
                mainPanel(
                  h4("Drug Effect (ALT) with Daily Dose", align = "center"),
                  plotlyOutput("plot"), align = "right"
                )
  )
))

server <- shinyServer(function(input, output) {
  
  alt_data <- read.csv("https://raw.githubusercontent.com/chang5307/test/master/test_alt.csv")
  alt_data <- alt_data %>% mutate(ResearchName = str_match(Bibliography, "(.*?),(.*?.)")[,1])
  alt_data$ResearchYear <- alt_data$Publicationyear
  alt_data$DailyDose[alt_data$UnitofDose=="mg/100g"] <- alt_data$DailyDose[alt_data$UnitofDose=="mg/100g"]*10
  alt_data$UnitofDose[alt_data$UnitofDose=="mg/100g"] <- "mg/kg"
  alt_data <- alt_data[alt_data$UnitofDose!="IU/L",]
  
  alt_data[alt_data$UnitofDose == "mg/day" & alt_data$Species == "Human", "DailyDose"] <- alt_data[alt_data$UnitofDose == "mg/day" & alt_data$Species == "Human", "DailyDose"]/60
  alt_data[alt_data$UnitofDose == "mg/day" & alt_data$Species == "Rat", "DailyDose"] <- alt_data[alt_data$UnitofDose == "mg/day" & alt_data$Species == "Rat", "DailyDose"]/0.15
  alt_data[alt_data$UnitofDose == "mg/day" & alt_data$Species == "Non-human primate", "DailyDose"] <-alt_data[alt_data$UnitofDose == "mg/day" & alt_data$Species == "Non-human primate", "DailyDose"]/3
  alt_data[alt_data$UnitofDose == "mg/day" & alt_data$Species == "Mouse", "DailyDose"] <-alt_data[alt_data$UnitofDose == "mg/day" & alt_data$Species == "Mouse", "DailyDose"]/0.02
  alt_data[alt_data$UnitofDose == "mg/day", "UnitofDose"] <- "mg/kg"
  
  alt_data[alt_data$UnitofDose == "mg" & alt_data$Species == "Rat", "DailyDose"] <- alt_data[alt_data$UnitofDose == "mg/day" & alt_data$Species == "Rat", "DailyDose"]/0.15
  alt_data[alt_data$UnitofDose == "mg" & alt_data$Species == "Mouse", "DailyDose"] <-alt_data[alt_data$UnitofDose == "mg/day" & alt_data$Species == "Mouse", "DailyDose"]/0.02
  alt_data[alt_data$UnitofDose == "mg", "UnitofDose"] <- "mg/kg"
  
  alt_data$Endpointunits[alt_data$Endpointunits == "IU/L"] <- "U/L"
  alt_data$Endpointunits[alt_data$Endpointunits == "U/l"] <- "U/L"
  alt_data$Endpointunits[alt_data$Endpointunits == "mmol/L"] <- "U/L"
  alt_data$Effectvalue[alt_data$Endpointunits == "nkat/L"] <- alt_data$Effectvalue[alt_data$Endpointunits == "nkat/L"]/0.01667
  alt_data$Endpointunits[alt_data$Endpointunits == "nkat/L"] <- "U/L"
  
  output$plot <- renderPlotly({
    new_data <- alt_data %>% filter(Drug %in% input$drug, Species %in% input$species, Endpointunits == input$altunit)
    options(scipen=999, "digits"=1)
    
    if (input$logdose %% 2 == 1 && length(input$drug)==2) {
      ggplot() +
        geom_point(new_data, mapping=aes(x=DailyDose, y=Effectvalue, color=Drug, shape=Species, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n","Study ID: ", study_ID))), size = 6) + 
        scale_color_manual(breaks = c("Rosiglitazone/Avandia", "Troglitazone/Rezulin"), values=c("green", "red")) +
        scale_shape_discrete(solid=F) +
        theme_bw() + theme_minimal() +
        scale_x_continuous(trans = "log1p",breaks = sort(unique(c(unique(new_data$DailyDose),10,100,1000)))) +
        labs(x = "Logarithmic Scale of DailyDose (mg/kg)", y = paste0("Effect Value (",input$altunit,")")) +
        theme(legend.text = element_text(size=8),
              legend.title = element_text(size=8),
              axis.title.x = element_text(size = 14),
              axis.text.x = element_text(angle = 45,
                                         colour = c(rep('black',length(unique(new_data$DailyDose[new_data$DailyDose<10]))),'red',
                                                    rep('black',length(unique(new_data$DailyDose[new_data$DailyDose>10&new_data$DailyDose<100]))),'red',
                                                    rep('black',length(unique(new_data$DailyDose[new_data$DailyDose>100&new_data$DailyDose<1000]))),'red',
                                                    rep('black',length(unique(new_data$DailyDose[new_data$DailyDose>1000]))))))
    } else if (input$logdose %% 2 == 0 && length(input$drug)==2) {
      ggplot() +
        geom_point(new_data, mapping=aes(x=DailyDose, y=Effectvalue, color=Drug, shape=Species, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n","Study ID: ", study_ID))), size = 6) + 
        scale_color_manual(breaks = c("Rosiglitazone/Avandia", "Troglitazone/Rezulin"), values=c("green", "red")) +
        scale_shape_discrete(solid=F) +
        theme_bw() + theme_minimal() +
        labs(x = "DailyDose (mg/kg)", y = paste0("Effect Value (",input$altunit,")")) +
        theme(axis.title.x = element_text(size = 14),
              axis.text.x = element_text(angle = 45),
              legend.title = element_text(size=8), 
              legend.text = element_text(size=8))
    } else if (input$logdose %% 2 == 1 && input$drug=="Rosiglitazone/Avandia") {
      ggplot() +
        geom_point(new_data, mapping=aes(x=DailyDose, y=Effectvalue, color=Drug, shape=Species, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n","Study ID: ", study_ID))), size = 6) + 
        scale_color_manual(breaks = c("Rosiglitazone/Avandia"), values=c("green")) +
        scale_shape_discrete(solid=F) +
        theme_bw() + theme_minimal() +
        scale_x_continuous(trans = "log1p",breaks = sort(unique(c(unique(new_data$DailyDose),10,100,1000)))) +
        labs(x = "Logarithmic Scale of DailyDose (mg/kg)", y = paste0("Effect Value (",input$altunit,")")) +
        theme(legend.text = element_text(size=8),
              legend.title = element_text(size=8),
              axis.title.x = element_text(size = 14),
              axis.text.x = element_text(angle = 45,
                                         colour = c(rep('black',length(unique(new_data$DailyDose[new_data$DailyDose<10]))),'red',
                                                    rep('black',length(unique(new_data$DailyDose[new_data$DailyDose>10&new_data$DailyDose<100]))),'red',
                                                    rep('black',length(unique(new_data$DailyDose[new_data$DailyDose>100&new_data$DailyDose<1000]))),'red',
                                                    rep('black',length(unique(new_data$DailyDose[new_data$DailyDose>1000]))))))
    } else if (input$logdose %% 2 == 0 && input$drug=="Rosiglitazone/Avandia") {
      ggplot() +
        geom_point(new_data, mapping=aes(x=DailyDose, y=Effectvalue, color=Drug, shape=Species, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n","Study ID: ", study_ID))), size = 6) + 
        scale_color_manual(breaks = c("Rosiglitazone/Avandia"), values=c("green")) +
        scale_shape_discrete(solid=F) +
        theme_bw() + theme_minimal() +
        labs(x = "DailyDose (mg/kg)", y = paste0("Effect Value (",input$altunit,")")) +
        theme(axis.title.x = element_text(size = 14),
              axis.text.x = element_text(angle = 45),
              legend.title = element_text(size=8), 
              legend.text = element_text(size=8))
    } else if (input$logdose %% 2 == 1 && input$drug=="Troglitazone/Rezulin") {
      ggplot() +
        geom_point(new_data, mapping=aes(x=DailyDose, y=Effectvalue, color=Drug, shape=Species, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n","Study ID: ", study_ID))), size = 6) + 
        scale_color_manual(breaks = c("Troglitazone/Rezulin"), values=c("red")) +
        scale_shape_discrete(solid=F) +
        theme_bw() + theme_minimal() +
        scale_x_continuous(trans = "log1p",breaks = sort(unique(c(unique(new_data$DailyDose),10,100,1000)))) +
        labs(x = "Logarithmic Scale of DailyDose (mg/kg)", y = paste0("Effect Value (",input$altunit,")")) +
        theme(legend.text = element_text(size=8),
              legend.title = element_text(size=8),
              axis.title.x = element_text(size = 14),
              axis.text.x = element_text(angle = 45,
                                         colour = c(rep('black',length(unique(new_data$DailyDose[new_data$DailyDose<10]))),'red',
                                                    rep('black',length(unique(new_data$DailyDose[new_data$DailyDose>10&new_data$DailyDose<100]))),'red',
                                                    rep('black',length(unique(new_data$DailyDose[new_data$DailyDose>100&new_data$DailyDose<1000]))),'red',
                                                    rep('black',length(unique(new_data$DailyDose[new_data$DailyDose>1000]))))))
    } else {
      ggplot() +
        geom_point(new_data, mapping=aes(x=DailyDose, y=Effectvalue, color=Drug, shape=Species, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n","Study ID: ", study_ID))), size = 6) + 
        scale_color_manual(breaks = c("Troglitazone/Rezulin"), values=c("red")) +
        scale_shape_discrete(solid=F) +
        theme_bw() + theme_minimal() +
        labs(x = "DailyDose (mg/kg)", y = paste0("Effect Value (",input$altunit,")")) +
        theme(axis.title.x = element_text(size = 14),
              axis.text.x = element_text(angle = 45),
              legend.title = element_text(size=8), 
              legend.text = element_text(size=8))
    }
    
  })
  
})

shinyApp(ui, server)