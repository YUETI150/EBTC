library(readxl)
library(ggplot2)
library(tidyverse)
library(plotly)
library(shiny)
library(stringr)

# import ALT data

alt_data <- read_excel("Excel file for ALT.xlsx")
alt_data_plot <- alt_data %>%
        select("Bibliography","Species", "Drug", "Endpointname","DailyDose", "UnitofDose", "Endpointunits",size = "Numberofexperimentalunits(animals,humans)", "Effectvalue", "Variability", "UnitofVariability", "study_ID")%>%
        filter(UnitofVariability == "SE" | UnitofVariability == "SEM" | UnitofVariability == "SD") %>%
        filter(Endpointname != "ALT >3 x ULN") %>%
        filter(size!= "na") %>%
        mutate(ResearchName = str_match(Bibliography, "(.*?), (.*?).")[, 1]) %>%
        mutate(ResearchYear = str_match(Bibliography, "\\d{4}"))
alt_data_plot <- alt_data_plot[-1]
alt_data_plot$Variability <- as.numeric(alt_data_plot$Variability)
alt_data_plot[alt_data_plot == "6-10"] <- 8
alt_data_plot$size <- as.numeric(alt_data_plot$size)
alt_data_plot[alt_data_plot == "SEM"] <- "SE"


alt_data_plot[alt_data_plot$UnitofVariability == "SD",]$Variability <- 
        (alt_data_plot[alt_data_plot$UnitofVariability == "SD",]$Variability)/
        sqrt(as.numeric(alt_data_plot[alt_data_plot$UnitofVariability == "SD",]$size))

alt_data_plot[alt_data_plot == "SD"] <- "SE"
alt_data_plot <- alt_data_plot[-c(82, 83, 84),]

alt_data_plot[alt_data_plot$DailyDose=="200-600", "DailyDose"]<- "400"
alt_data_plot[alt_data_plot$DailyDose=="4.4+/-2", "DailyDose"]<- "4.4"
alt_data_plot[alt_data_plot$DailyDose=="2 to 8", "DailyDose"]<- "5"


alt_data_plot[alt_data_plot$UnitofDose=="mg/100g", ]<- alt_data_plot %>% filter(UnitofDose == "mg/100g") %>%
        mutate(DailyDose = as.numeric(DailyDose) * 10, UnitofDose = "mg/kg")



#alt_data_plot <- read_excel("Bubble_Cleaned.xlsx")
alt_data_plot$DailyDose <- as.numeric(alt_data_plot$DailyDose)
alt_data_plot$size <- as.numeric(alt_data_plot$size)
alt_data_plot$Effectvalue <- as.numeric(alt_data_plot$Effectvalue)

alt_data_plot <- alt_data_plot[!alt_data_plot$UnitofDose == "IU/L",]
#convert g to mg/kg
alt_data_plot[alt_data_plot$UnitofDose == "g", "UnitofDose"] <- "mg" #only one row whose dailydose for rat is 10 g, it should be 10 mg, or its extremely unusual
alt_data_plot[alt_data_plot$UnitofDose == "mg" & alt_data_plot$Species == "Human", "DailyDose"] <- alt_data_plot[alt_data_plot$UnitofDose == "mg" & alt_data_plot$Species == "Human", "DailyDose"]/60
alt_data_plot[alt_data_plot$UnitofDose == "mg" & alt_data_plot$Species == "Rat", "DailyDose"] <- alt_data_plot[alt_data_plot$UnitofDose == "mg" & alt_data_plot$Species == "Rat", "DailyDose"]/0.15
alt_data_plot[alt_data_plot$UnitofDose == "mg" & alt_data_plot$Species == "Non-human primate", "DailyDose"] <-alt_data_plot[alt_data_plot$UnitofDose == "mg" & alt_data_plot$Species == "Non-human primate", "DailyDose"]/3
alt_data_plot[alt_data_plot$UnitofDose == "mg", "UnitofDose"] <- "mg/kg"
#convert mg/100g to mg/kg
alt_data_plot[alt_data_plot$UnitofDose == "mg/100g","DailyDose"] <- alt_data_plot[alt_data_plot$UnitofDose == "mg/100g","DailyDose"]*10
alt_data_plot[alt_data_plot$UnitofDose == "mg/100g", "UnitofDose"] <- "mg/kg"
#convert mg/kg/diet to mg/kg
alt_data_plot[alt_data_plot$UnitofDose == "mg/kg/diet", "UnitofDose"] <- "mg/kg"
#convert mg/day to mg/kg
alt_data_plot[alt_data_plot$UnitofDose == "mg/day" & alt_data_plot$Species == "Human", "DailyDose"] <- alt_data_plot[alt_data_plot$UnitofDose == "mg/day" & alt_data_plot$Species == "Human", "DailyDose"]/60
alt_data_plot[alt_data_plot$UnitofDose == "mg/day" & alt_data_plot$Species == "Rat", "DailyDose"] <- alt_data_plot[alt_data_plot$UnitofDose == "mg/day" & alt_data_plot$Species == "Rat", "DailyDose"]/0.15
alt_data_plot[alt_data_plot$UnitofDose == "mg/day" & alt_data_plot$Species == "Non-human primate", "DailyDose"] <-alt_data_plot[alt_data_plot$UnitofDose == "mg/day" & alt_data_plot$Species == "Non-human primate", "DailyDose"]/3
alt_data_plot[alt_data_plot$UnitofDose == "mg/day" & alt_data_plot$Species == "Mouse", "DailyDose"] <-alt_data_plot[alt_data_plot$UnitofDose == "mg/day" & alt_data_plot$Species == "Mouse", "DailyDose"]/0.02
alt_data_plot[alt_data_plot$UnitofDose == "mg/day", "UnitofDose"] <- "mg/kg"

alt_data_plot <- alt_data_plot[order(alt_data_plot$DailyDose, decreasing = FALSE),]

alt_data_plot$Endpointunits[alt_data_plot$Endpointunits == "U/l"] <- "U/L"
alt_data_plot$Endpointunits[alt_data_plot$Endpointunits == "IU/L"] <- "U/L"
alt_data_plot$Endpointunits[alt_data_plot$Endpointunits == "mmol/L"] <- "U/L"

alt_data_plot$DailyDose <- round(alt_data_plot$DailyDose, digits = 2)

server <- function(input, output) {
        output$plot <- renderPlotly({
                options(scipen = 100)
                
                if(input$species %in% c("Mouse", "Rat", "Human", "Non-human primate")){  
                        new_data2 <- alt_data_plot %>% filter(Drug %in% input$drug, Endpointunits %in% input$Endpointunits, Species %in% input$species)
                        validate(
                                need(input$Endpointunits %in% new_data2$Endpointunits[new_data2$Endpointunits == input$Endpointunits], "Data doesn't exist, please select others")
                        )
                        
                        
                        y_title2 <- paste("Effectvalue (", new_data2$Endpointunits, ")")
                        
                        
                        
                        x_ticks <- c(sort(unique(c(10, 100, 1000,unique(new_data2$DailyDose)))))
                        if (input$BubbleSize == "Sample size" & input$`Logarithmic Scale` == TRUE) {
                                ggplotly(ggplot(new_data2, aes(x=DailyDose, y=Effectvalue, color=Drug, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n", "Study ID: ", study_ID)))) + 
                                                 geom_point( alpha = 0.3,  size =  2 * sqrt(new_data2$size)) + theme_bw() + theme_minimal()+ scale_x_continuous( trans = "log1p", breaks=x_ticks)+
                                                 theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") + xlab("logarithmic x-axis (mg/kg)")+ ylab(y_title2)+
                                                 scale_color_manual(
                                                         breaks = c("Rosiglitazone/Avandia", "Troglitazone/Rezulin"),
                                                         values=c("green", "red")) 
                                )
                                
                                
                                
                                
                        } else if(input$BubbleSize == "Sample size" & input$`Logarithmic Scale` == FALSE) {
                                ggplotly( ggplot(new_data2, aes(x=DailyDose, y=Effectvalue, color=Drug, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n", "Study ID: ", study_ID)))) + 
                                                  geom_point( alpha = 0.3, size = 2 * sqrt(new_data2$size)) + theme_bw() + theme_minimal()+expand_limits(x=0)+theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")+
                                                  xlab("DailyDose (mg/kg)")  + ylab(y_title2)+
                                                  scale_color_manual(
                                                          breaks = c("Rosiglitazone/Avandia", "Troglitazone/Rezulin"),
                                                          values=c("green", "red"))) 
                                
                                
                        }else if(input$BubbleSize == "SE" & input$`Logarithmic Scale` == TRUE){
                                ggplotly(ggplot(new_data2, aes(x=DailyDose, y=Effectvalue, color=Drug, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n", "Study ID: ", study_ID)))) + 
                                                 geom_point( alpha = 0.3, size = 30 /new_data2$Variability) + theme_bw() + theme_minimal()+ scale_x_continuous( trans = "log1p", breaks=x_ticks)+
                                                 theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "top") + xlab("logarithmic x-axis (mg/kg)")  +ylab(y_title2)+
                                                 scale_color_manual(
                                                         breaks = c("Rosiglitazone/Avandia", "Troglitazone/Rezulin"),
                                                         values=c("green", "red")))
                                
                                
                        }else {
                                ggplotly(ggplot(new_data2, aes(x=DailyDose, y=Effectvalue, color=Drug, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n", "Study ID: ", study_ID)))) + 
                                                 geom_point(alpha = 0.3, size =  30/new_data2$Variability) + theme_bw() + theme_minimal()+expand_limits(x=0)+
                                                 theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") + xlab("DailyDose (mg/kg)")   +ylab(y_title2)+
                                                 scale_color_manual(
                                                         breaks = c("Rosiglitazone/Avandia", "Troglitazone/Rezulin"),
                                                         values=c("green", "red")))
                                
                        }  
                        
                        
                        
                        
                        
                        
                } else if (input$species == "All"){
                        new_data <- alt_data_plot %>% filter(Drug %in% input$drug, Endpointunits %in% input$Endpointunits)
                        validate(
                                need(input$Endpointunits %in% new_data$Endpointunits[new_data$Endpointunits == input$Endpointunits], "Data doesn't exist, please select others")
                        )
                        y_title <- if(new_data$Endpointunits == new_data$Endpointunits){paste("Effectvalue (", new_data$Endpointunits, ")")} else {paste("No data")};
                        
                        
                        x_ticks <- c(sort(unique(c(10, 100, 1000,unique(new_data$DailyDose)))))
                        if (input$BubbleSize == "Sample size" & input$`Logarithmic Scale` == TRUE) {
                                ggplotly(ggplot(new_data, aes(x=DailyDose, y=Effectvalue, color=Drug, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n", "Study ID: ", study_ID)))) + 
                                                 geom_point( alpha = 0.3, size =  2 * sqrt(new_data$size)) + theme_bw() + theme_minimal()+ scale_x_continuous( trans = "log1p", breaks=x_ticks)+
                                                 theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") + xlab("logarithmic x-axis (mg/kg)") +ylab(y_title)+
                                                 scale_color_manual(
                                                         breaks = c("Rosiglitazone/Avandia", "Troglitazone/Rezulin"),
                                                         values=c("green", "red")))
                                
                                
                        } else if(input$BubbleSize == "Sample size" & input$`Logarithmic Scale` == FALSE) {
                                ggplotly(ggplot(new_data, aes(x=DailyDose, y=Effectvalue, color=Drug, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n", "Study ID: ", study_ID)))) + 
                                                 geom_point( alpha = 0.3, size = 2 * sqrt(new_data$size)) + theme_bw() + theme_minimal()+expand_limits(x=0)+theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")+
                                                 xlab("DailyDose (mg/kg)") + ylab(y_title)+
                                                 scale_color_manual(
                                                         breaks = c("Rosiglitazone/Avandia", "Troglitazone/Rezulin"),
                                                         values=c("green", "red")))
                                
                                
                        }else if(input$BubbleSize == "SE" & input$`Logarithmic Scale` == TRUE){
                                ggplotly( ggplot(new_data, aes(x=DailyDose, y=Effectvalue, color=Drug, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n", "Study ID: ", study_ID)))) + 
                                                  geom_point( alpha = 0.3, size = 30 /new_data$Variability) + theme_bw() + theme_minimal()+ scale_x_continuous( trans = "log1p", breaks=x_ticks)+
                                                  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") + xlab("logarithmic x-axis (mg/kg)")+ ylab(y_title)+
                                                  scale_color_manual(
                                                          breaks = c("Rosiglitazone/Avandia", "Troglitazone/Rezulin"),
                                                          values=c("green", "red")))
                                
                                
                        }else {
                                ggplotly(ggplot(new_data, aes(x=DailyDose, y=Effectvalue, color=Drug, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n", "Study ID: ", study_ID)))) + 
                                                 geom_point( alpha = 0.3, size =  30/new_data$Variability) + theme_bw() + theme_minimal()+expand_limits(x=0)+
                                                 theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") + xlab("DailyDose (mg/kg)") + ylab(y_title)+
                                                 scale_color_manual(
                                                         breaks = c("Rosiglitazone/Avandia", "Troglitazone/Rezulin"),
                                                         values=c("green", "red")))
                        }
                }
        }
        
        )
        
}

ui <- fluidPage(
        titlePanel("EBTC Tox21 Systematic Review Results: The Biomedical Research in Bubble Plot"),
        br(),
        br(),
        sidebarLayout(position = "left", 
                      sidebarPanel(width = 3,
                                   selectInput("species", label = h4("Select Species"),
                                               choices = list("All"="All",
                                                              "Mouse"="Mouse", 
                                                              "Rat"="Rat",
                                                              "Human"="Human",
                                                              "Non-human primate"="Non-human primate"),
                                               selected = "All"),
                                   
                                   
                                   selectInput("Endpointunits", label = h4("Endpoint Units"),
                                               choices = c("U/L"="U/L",
                                                           "IUA"="IUA",
                                                           "counts" = "counts"),
                                               selected = "U/L"),
                                   
                                   checkboxGroupInput("drug", label = h4("Select Drugs"),
                                                      choices = list("Rosiglitazone/Avandia" = "Rosiglitazone/Avandia",
                                                                     "Troglitazone/Rezulin" = "Troglitazone/Rezulin"),
                                                      selected = "Rosiglitazone/Avandia"),
                                   
                                   selectInput("BubbleSize", label = h4("Criteria for Bubble Size"),
                                               choices = c("SE"="SE",
                                                           "Sample size"="Sample size"),
                                               selected = "SE"),
                                   
                                   br(),
                                   h4("Logarithmic Scale"),
                                   checkboxInput("Logarithmic Scale", label = "Check to transform"),
                                   
                                   
                                   br()
                                   
                      ),
                      mainPanel(
                              h4("Drug Effect (ALT) with Daily Dose", align = "center"),
                              plotlyOutput("plot", width = "105%", height = 700), align = "right"
                      )
        ),
        plotlyOutput("server.R")
)

shinyApp(ui = ui, server = server)

